{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CachePolicySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicySummary where

import Network.AWS.CloudFront.Types.CachePolicy
import Network.AWS.CloudFront.Types.CachePolicyType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains a cache policy.
--
--
--
-- /See:/ 'cachePolicySummary' smart constructor.
data CachePolicySummary = CachePolicySummary'
  { _cpsType ::
      !CachePolicyType,
    _cpsCachePolicy :: !CachePolicy
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CachePolicySummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpsType' - The type of cache policy, either @managed@ (created by AWS) or @custom@ (created in this AWS account).
--
-- * 'cpsCachePolicy' - The cache policy.
cachePolicySummary ::
  -- | 'cpsType'
  CachePolicyType ->
  -- | 'cpsCachePolicy'
  CachePolicy ->
  CachePolicySummary
cachePolicySummary pType_ pCachePolicy_ =
  CachePolicySummary'
    { _cpsType = pType_,
      _cpsCachePolicy = pCachePolicy_
    }

-- | The type of cache policy, either @managed@ (created by AWS) or @custom@ (created in this AWS account).
cpsType :: Lens' CachePolicySummary CachePolicyType
cpsType = lens _cpsType (\s a -> s {_cpsType = a})

-- | The cache policy.
cpsCachePolicy :: Lens' CachePolicySummary CachePolicy
cpsCachePolicy = lens _cpsCachePolicy (\s a -> s {_cpsCachePolicy = a})

instance FromXML CachePolicySummary where
  parseXML x =
    CachePolicySummary' <$> (x .@ "Type") <*> (x .@ "CachePolicy")

instance Hashable CachePolicySummary

instance NFData CachePolicySummary
