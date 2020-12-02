{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.PipelineDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.PipelineDescription where

import Network.AWS.DataPipeline.Types.Field
import Network.AWS.DataPipeline.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains pipeline metadata.
--
--
--
-- /See:/ 'pipelineDescription' smart constructor.
data PipelineDescription = PipelineDescription'
  { _pdDescription ::
      !(Maybe Text),
    _pdTags :: !(Maybe [Tag]),
    _pdPipelineId :: !Text,
    _pdName :: !Text,
    _pdFields :: ![Field]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PipelineDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdDescription' - Description of the pipeline.
--
-- * 'pdTags' - A list of tags to associated with a pipeline. Tags let you control access to pipelines. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html Controlling User Access to Pipelines> in the /AWS Data Pipeline Developer Guide/ .
--
-- * 'pdPipelineId' - The pipeline identifier that was assigned by AWS Data Pipeline. This is a string of the form @df-297EG78HU43EEXAMPLE@ .
--
-- * 'pdName' - The name of the pipeline.
--
-- * 'pdFields' - A list of read-only fields that contain metadata about the pipeline: @userId, @accountId, and @pipelineState.
pipelineDescription ::
  -- | 'pdPipelineId'
  Text ->
  -- | 'pdName'
  Text ->
  PipelineDescription
pipelineDescription pPipelineId_ pName_ =
  PipelineDescription'
    { _pdDescription = Nothing,
      _pdTags = Nothing,
      _pdPipelineId = pPipelineId_,
      _pdName = pName_,
      _pdFields = mempty
    }

-- | Description of the pipeline.
pdDescription :: Lens' PipelineDescription (Maybe Text)
pdDescription = lens _pdDescription (\s a -> s {_pdDescription = a})

-- | A list of tags to associated with a pipeline. Tags let you control access to pipelines. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html Controlling User Access to Pipelines> in the /AWS Data Pipeline Developer Guide/ .
pdTags :: Lens' PipelineDescription [Tag]
pdTags = lens _pdTags (\s a -> s {_pdTags = a}) . _Default . _Coerce

-- | The pipeline identifier that was assigned by AWS Data Pipeline. This is a string of the form @df-297EG78HU43EEXAMPLE@ .
pdPipelineId :: Lens' PipelineDescription Text
pdPipelineId = lens _pdPipelineId (\s a -> s {_pdPipelineId = a})

-- | The name of the pipeline.
pdName :: Lens' PipelineDescription Text
pdName = lens _pdName (\s a -> s {_pdName = a})

-- | A list of read-only fields that contain metadata about the pipeline: @userId, @accountId, and @pipelineState.
pdFields :: Lens' PipelineDescription [Field]
pdFields = lens _pdFields (\s a -> s {_pdFields = a}) . _Coerce

instance FromJSON PipelineDescription where
  parseJSON =
    withObject
      "PipelineDescription"
      ( \x ->
          PipelineDescription'
            <$> (x .:? "description")
            <*> (x .:? "tags" .!= mempty)
            <*> (x .: "pipelineId")
            <*> (x .: "name")
            <*> (x .:? "fields" .!= mempty)
      )

instance Hashable PipelineDescription

instance NFData PipelineDescription
