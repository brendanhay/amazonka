{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.PolicyStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.PolicyStatus where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | The container element for a bucket's policy status.
--
--
--
-- /See:/ 'policyStatus' smart constructor.
newtype PolicyStatus = PolicyStatus' {_psIsPublic :: Maybe Bool}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PolicyStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psIsPublic' - The policy status for this bucket. @TRUE@ indicates that this bucket is public. @FALSE@ indicates that the bucket is not public.
policyStatus ::
  PolicyStatus
policyStatus = PolicyStatus' {_psIsPublic = Nothing}

-- | The policy status for this bucket. @TRUE@ indicates that this bucket is public. @FALSE@ indicates that the bucket is not public.
psIsPublic :: Lens' PolicyStatus (Maybe Bool)
psIsPublic = lens _psIsPublic (\s a -> s {_psIsPublic = a})

instance FromXML PolicyStatus where
  parseXML x = PolicyStatus' <$> (x .@? "IsPublic")

instance Hashable PolicyStatus

instance NFData PolicyStatus
