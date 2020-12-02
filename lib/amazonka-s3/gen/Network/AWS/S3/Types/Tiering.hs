{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Tiering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Tiering where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.IntelligentTieringAccessTier

-- | The S3 Intelligent-Tiering storage class is designed to optimize storage costs by automatically moving data to the most cost-effective storage access tier, without additional operational overhead.
--
--
--
-- /See:/ 'tiering' smart constructor.
data Tiering = Tiering'
  { _tDays :: !Int,
    _tAccessTier :: !IntelligentTieringAccessTier
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Tiering' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tDays' - The number of consecutive days of no access after which an object will be eligible to be transitioned to the corresponding tier. The minimum number of days specified for Archive Access tier must be at least 90 days and Deep Archive Access tier must be at least 180 days. The maximum can be up to 2 years (730 days).
--
-- * 'tAccessTier' - S3 Intelligent-Tiering access tier. See <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects> for a list of access tiers in the S3 Intelligent-Tiering storage class.
tiering ::
  -- | 'tDays'
  Int ->
  -- | 'tAccessTier'
  IntelligentTieringAccessTier ->
  Tiering
tiering pDays_ pAccessTier_ =
  Tiering' {_tDays = pDays_, _tAccessTier = pAccessTier_}

-- | The number of consecutive days of no access after which an object will be eligible to be transitioned to the corresponding tier. The minimum number of days specified for Archive Access tier must be at least 90 days and Deep Archive Access tier must be at least 180 days. The maximum can be up to 2 years (730 days).
tDays :: Lens' Tiering Int
tDays = lens _tDays (\s a -> s {_tDays = a})

-- | S3 Intelligent-Tiering access tier. See <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects> for a list of access tiers in the S3 Intelligent-Tiering storage class.
tAccessTier :: Lens' Tiering IntelligentTieringAccessTier
tAccessTier = lens _tAccessTier (\s a -> s {_tAccessTier = a})

instance FromXML Tiering where
  parseXML x = Tiering' <$> (x .@ "Days") <*> (x .@ "AccessTier")

instance Hashable Tiering

instance NFData Tiering

instance ToXML Tiering where
  toXML Tiering' {..} =
    mconcat ["Days" @= _tDays, "AccessTier" @= _tAccessTier]
