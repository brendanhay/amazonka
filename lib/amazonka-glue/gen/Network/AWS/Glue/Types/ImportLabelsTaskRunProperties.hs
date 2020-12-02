{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ImportLabelsTaskRunProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ImportLabelsTaskRunProperties where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies configuration properties for an importing labels task run.
--
--
--
-- /See:/ 'importLabelsTaskRunProperties' smart constructor.
data ImportLabelsTaskRunProperties = ImportLabelsTaskRunProperties'
  { _iltrpReplace ::
      !(Maybe Bool),
    _iltrpInputS3Path ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImportLabelsTaskRunProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iltrpReplace' - Indicates whether to overwrite your existing labels.
--
-- * 'iltrpInputS3Path' - The Amazon Simple Storage Service (Amazon S3) path from where you will import the labels.
importLabelsTaskRunProperties ::
  ImportLabelsTaskRunProperties
importLabelsTaskRunProperties =
  ImportLabelsTaskRunProperties'
    { _iltrpReplace = Nothing,
      _iltrpInputS3Path = Nothing
    }

-- | Indicates whether to overwrite your existing labels.
iltrpReplace :: Lens' ImportLabelsTaskRunProperties (Maybe Bool)
iltrpReplace = lens _iltrpReplace (\s a -> s {_iltrpReplace = a})

-- | The Amazon Simple Storage Service (Amazon S3) path from where you will import the labels.
iltrpInputS3Path :: Lens' ImportLabelsTaskRunProperties (Maybe Text)
iltrpInputS3Path = lens _iltrpInputS3Path (\s a -> s {_iltrpInputS3Path = a})

instance FromJSON ImportLabelsTaskRunProperties where
  parseJSON =
    withObject
      "ImportLabelsTaskRunProperties"
      ( \x ->
          ImportLabelsTaskRunProperties'
            <$> (x .:? "Replace") <*> (x .:? "InputS3Path")
      )

instance Hashable ImportLabelsTaskRunProperties

instance NFData ImportLabelsTaskRunProperties
