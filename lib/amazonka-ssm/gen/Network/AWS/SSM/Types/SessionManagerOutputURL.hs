{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.SessionManagerOutputURL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.SessionManagerOutputURL where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Reserved for future use.
--
--
--
-- /See:/ 'sessionManagerOutputURL' smart constructor.
data SessionManagerOutputURL = SessionManagerOutputURL'
  { _smouS3OutputURL ::
      !(Maybe Text),
    _smouCloudWatchOutputURL :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SessionManagerOutputURL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smouS3OutputURL' - Reserved for future use.
--
-- * 'smouCloudWatchOutputURL' - Reserved for future use.
sessionManagerOutputURL ::
  SessionManagerOutputURL
sessionManagerOutputURL =
  SessionManagerOutputURL'
    { _smouS3OutputURL = Nothing,
      _smouCloudWatchOutputURL = Nothing
    }

-- | Reserved for future use.
smouS3OutputURL :: Lens' SessionManagerOutputURL (Maybe Text)
smouS3OutputURL = lens _smouS3OutputURL (\s a -> s {_smouS3OutputURL = a})

-- | Reserved for future use.
smouCloudWatchOutputURL :: Lens' SessionManagerOutputURL (Maybe Text)
smouCloudWatchOutputURL = lens _smouCloudWatchOutputURL (\s a -> s {_smouCloudWatchOutputURL = a})

instance FromJSON SessionManagerOutputURL where
  parseJSON =
    withObject
      "SessionManagerOutputURL"
      ( \x ->
          SessionManagerOutputURL'
            <$> (x .:? "S3OutputUrl") <*> (x .:? "CloudWatchOutputUrl")
      )

instance Hashable SessionManagerOutputURL

instance NFData SessionManagerOutputURL
