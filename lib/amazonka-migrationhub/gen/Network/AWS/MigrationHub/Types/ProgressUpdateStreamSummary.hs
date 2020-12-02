{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.ProgressUpdateStreamSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.ProgressUpdateStreamSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Summary of the AWS resource used for access control that is implicitly linked to your AWS account.
--
--
--
-- /See:/ 'progressUpdateStreamSummary' smart constructor.
newtype ProgressUpdateStreamSummary = ProgressUpdateStreamSummary'
  { _pussProgressUpdateStreamName ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProgressUpdateStreamSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pussProgressUpdateStreamName' - The name of the ProgressUpdateStream. /Do not store personal data in this field./
progressUpdateStreamSummary ::
  ProgressUpdateStreamSummary
progressUpdateStreamSummary =
  ProgressUpdateStreamSummary'
    { _pussProgressUpdateStreamName =
        Nothing
    }

-- | The name of the ProgressUpdateStream. /Do not store personal data in this field./
pussProgressUpdateStreamName :: Lens' ProgressUpdateStreamSummary (Maybe Text)
pussProgressUpdateStreamName = lens _pussProgressUpdateStreamName (\s a -> s {_pussProgressUpdateStreamName = a})

instance FromJSON ProgressUpdateStreamSummary where
  parseJSON =
    withObject
      "ProgressUpdateStreamSummary"
      ( \x ->
          ProgressUpdateStreamSummary'
            <$> (x .:? "ProgressUpdateStreamName")
      )

instance Hashable ProgressUpdateStreamSummary

instance NFData ProgressUpdateStreamSummary
