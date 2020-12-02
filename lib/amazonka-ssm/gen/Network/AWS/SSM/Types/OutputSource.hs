{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OutputSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OutputSource where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the source where the association execution details are stored.
--
--
--
-- /See:/ 'outputSource' smart constructor.
data OutputSource = OutputSource'
  { _osOutputSourceId ::
      !(Maybe Text),
    _osOutputSourceType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osOutputSourceId' - The ID of the output source, for example the URL of an S3 bucket.
--
-- * 'osOutputSourceType' - The type of source where the association execution details are stored, for example, Amazon S3.
outputSource ::
  OutputSource
outputSource =
  OutputSource'
    { _osOutputSourceId = Nothing,
      _osOutputSourceType = Nothing
    }

-- | The ID of the output source, for example the URL of an S3 bucket.
osOutputSourceId :: Lens' OutputSource (Maybe Text)
osOutputSourceId = lens _osOutputSourceId (\s a -> s {_osOutputSourceId = a})

-- | The type of source where the association execution details are stored, for example, Amazon S3.
osOutputSourceType :: Lens' OutputSource (Maybe Text)
osOutputSourceType = lens _osOutputSourceType (\s a -> s {_osOutputSourceType = a})

instance FromJSON OutputSource where
  parseJSON =
    withObject
      "OutputSource"
      ( \x ->
          OutputSource'
            <$> (x .:? "OutputSourceId") <*> (x .:? "OutputSourceType")
      )

instance Hashable OutputSource

instance NFData OutputSource
