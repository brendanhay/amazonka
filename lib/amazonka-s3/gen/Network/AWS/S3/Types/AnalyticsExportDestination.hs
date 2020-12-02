{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.AnalyticsExportDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.AnalyticsExportDestination where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.AnalyticsS3BucketDestination

-- | Where to publish the analytics results.
--
--
--
-- /See:/ 'analyticsExportDestination' smart constructor.
newtype AnalyticsExportDestination = AnalyticsExportDestination'
  { _aedS3BucketDestination ::
      AnalyticsS3BucketDestination
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AnalyticsExportDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aedS3BucketDestination' - A destination signifying output to an S3 bucket.
analyticsExportDestination ::
  -- | 'aedS3BucketDestination'
  AnalyticsS3BucketDestination ->
  AnalyticsExportDestination
analyticsExportDestination pS3BucketDestination_ =
  AnalyticsExportDestination'
    { _aedS3BucketDestination =
        pS3BucketDestination_
    }

-- | A destination signifying output to an S3 bucket.
aedS3BucketDestination :: Lens' AnalyticsExportDestination AnalyticsS3BucketDestination
aedS3BucketDestination = lens _aedS3BucketDestination (\s a -> s {_aedS3BucketDestination = a})

instance FromXML AnalyticsExportDestination where
  parseXML x =
    AnalyticsExportDestination' <$> (x .@ "S3BucketDestination")

instance Hashable AnalyticsExportDestination

instance NFData AnalyticsExportDestination

instance ToXML AnalyticsExportDestination where
  toXML AnalyticsExportDestination' {..} =
    mconcat ["S3BucketDestination" @= _aedS3BucketDestination]
