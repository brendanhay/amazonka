{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ExportLabelsTaskRunProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ExportLabelsTaskRunProperties where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies configuration properties for an exporting labels task run.
--
--
--
-- /See:/ 'exportLabelsTaskRunProperties' smart constructor.
newtype ExportLabelsTaskRunProperties = ExportLabelsTaskRunProperties'
  { _eltrpOutputS3Path ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExportLabelsTaskRunProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eltrpOutputS3Path' - The Amazon Simple Storage Service (Amazon S3) path where you will export the labels.
exportLabelsTaskRunProperties ::
  ExportLabelsTaskRunProperties
exportLabelsTaskRunProperties =
  ExportLabelsTaskRunProperties' {_eltrpOutputS3Path = Nothing}

-- | The Amazon Simple Storage Service (Amazon S3) path where you will export the labels.
eltrpOutputS3Path :: Lens' ExportLabelsTaskRunProperties (Maybe Text)
eltrpOutputS3Path = lens _eltrpOutputS3Path (\s a -> s {_eltrpOutputS3Path = a})

instance FromJSON ExportLabelsTaskRunProperties where
  parseJSON =
    withObject
      "ExportLabelsTaskRunProperties"
      (\x -> ExportLabelsTaskRunProperties' <$> (x .:? "OutputS3Path"))

instance Hashable ExportLabelsTaskRunProperties

instance NFData ExportLabelsTaskRunProperties
