{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicySummary where

import Network.AWS.CloudFront.Types.OriginRequestPolicy
import Network.AWS.CloudFront.Types.OriginRequestPolicyType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains an origin request policy.
--
--
--
-- /See:/ 'originRequestPolicySummary' smart constructor.
data OriginRequestPolicySummary = OriginRequestPolicySummary'
  { _orpsType ::
      !OriginRequestPolicyType,
    _orpsOriginRequestPolicy ::
      !OriginRequestPolicy
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OriginRequestPolicySummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'orpsType' - The type of origin request policy, either @managed@ (created by AWS) or @custom@ (created in this AWS account).
--
-- * 'orpsOriginRequestPolicy' - The origin request policy.
originRequestPolicySummary ::
  -- | 'orpsType'
  OriginRequestPolicyType ->
  -- | 'orpsOriginRequestPolicy'
  OriginRequestPolicy ->
  OriginRequestPolicySummary
originRequestPolicySummary pType_ pOriginRequestPolicy_ =
  OriginRequestPolicySummary'
    { _orpsType = pType_,
      _orpsOriginRequestPolicy = pOriginRequestPolicy_
    }

-- | The type of origin request policy, either @managed@ (created by AWS) or @custom@ (created in this AWS account).
orpsType :: Lens' OriginRequestPolicySummary OriginRequestPolicyType
orpsType = lens _orpsType (\s a -> s {_orpsType = a})

-- | The origin request policy.
orpsOriginRequestPolicy :: Lens' OriginRequestPolicySummary OriginRequestPolicy
orpsOriginRequestPolicy = lens _orpsOriginRequestPolicy (\s a -> s {_orpsOriginRequestPolicy = a})

instance FromXML OriginRequestPolicySummary where
  parseXML x =
    OriginRequestPolicySummary'
      <$> (x .@ "Type") <*> (x .@ "OriginRequestPolicy")

instance Hashable OriginRequestPolicySummary

instance NFData OriginRequestPolicySummary
