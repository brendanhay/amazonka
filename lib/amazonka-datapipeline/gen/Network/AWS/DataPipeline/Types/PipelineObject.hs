{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.PipelineObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.PipelineObject where

import Network.AWS.DataPipeline.Types.Field
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a pipeline object. This can be a logical, physical, or physical attempt pipeline object. The complete set of components of a pipeline defines the pipeline.
--
--
--
-- /See:/ 'pipelineObject' smart constructor.
data PipelineObject = PipelineObject'
  { _pId :: !Text,
    _pName :: !Text,
    _pFields :: ![Field]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PipelineObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pId' - The ID of the object.
--
-- * 'pName' - The name of the object.
--
-- * 'pFields' - Key-value pairs that define the properties of the object.
pipelineObject ::
  -- | 'pId'
  Text ->
  -- | 'pName'
  Text ->
  PipelineObject
pipelineObject pId_ pName_ =
  PipelineObject' {_pId = pId_, _pName = pName_, _pFields = mempty}

-- | The ID of the object.
pId :: Lens' PipelineObject Text
pId = lens _pId (\s a -> s {_pId = a})

-- | The name of the object.
pName :: Lens' PipelineObject Text
pName = lens _pName (\s a -> s {_pName = a})

-- | Key-value pairs that define the properties of the object.
pFields :: Lens' PipelineObject [Field]
pFields = lens _pFields (\s a -> s {_pFields = a}) . _Coerce

instance FromJSON PipelineObject where
  parseJSON =
    withObject
      "PipelineObject"
      ( \x ->
          PipelineObject'
            <$> (x .: "id") <*> (x .: "name") <*> (x .:? "fields" .!= mempty)
      )

instance Hashable PipelineObject

instance NFData PipelineObject

instance ToJSON PipelineObject where
  toJSON PipelineObject' {..} =
    object
      ( catMaybes
          [ Just ("id" .= _pId),
            Just ("name" .= _pName),
            Just ("fields" .= _pFields)
          ]
      )
