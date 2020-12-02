{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.LabelingSetGenerationTaskRunProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.LabelingSetGenerationTaskRunProperties where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies configuration properties for a labeling set generation task run.
--
--
--
-- /See:/ 'labelingSetGenerationTaskRunProperties' smart constructor.
newtype LabelingSetGenerationTaskRunProperties = LabelingSetGenerationTaskRunProperties'
  { _lsgtrpOutputS3Path ::
      Maybe Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'LabelingSetGenerationTaskRunProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsgtrpOutputS3Path' - The Amazon Simple Storage Service (Amazon S3) path where you will generate the labeling set.
labelingSetGenerationTaskRunProperties ::
  LabelingSetGenerationTaskRunProperties
labelingSetGenerationTaskRunProperties =
  LabelingSetGenerationTaskRunProperties'
    { _lsgtrpOutputS3Path =
        Nothing
    }

-- | The Amazon Simple Storage Service (Amazon S3) path where you will generate the labeling set.
lsgtrpOutputS3Path :: Lens' LabelingSetGenerationTaskRunProperties (Maybe Text)
lsgtrpOutputS3Path = lens _lsgtrpOutputS3Path (\s a -> s {_lsgtrpOutputS3Path = a})

instance FromJSON LabelingSetGenerationTaskRunProperties where
  parseJSON =
    withObject
      "LabelingSetGenerationTaskRunProperties"
      ( \x ->
          LabelingSetGenerationTaskRunProperties' <$> (x .:? "OutputS3Path")
      )

instance Hashable LabelingSetGenerationTaskRunProperties

instance NFData LabelingSetGenerationTaskRunProperties
