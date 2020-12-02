{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.OrganizationsDecisionDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.OrganizationsDecisionDetail where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the effect that Organizations has on a policy simulation.
--
--
--
-- /See:/ 'organizationsDecisionDetail' smart constructor.
newtype OrganizationsDecisionDetail = OrganizationsDecisionDetail'
  { _oddAllowedByOrganizations ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OrganizationsDecisionDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oddAllowedByOrganizations' - Specifies whether the simulated operation is allowed by the Organizations service control policies that impact the simulated user's account.
organizationsDecisionDetail ::
  OrganizationsDecisionDetail
organizationsDecisionDetail =
  OrganizationsDecisionDetail'
    { _oddAllowedByOrganizations =
        Nothing
    }

-- | Specifies whether the simulated operation is allowed by the Organizations service control policies that impact the simulated user's account.
oddAllowedByOrganizations :: Lens' OrganizationsDecisionDetail (Maybe Bool)
oddAllowedByOrganizations = lens _oddAllowedByOrganizations (\s a -> s {_oddAllowedByOrganizations = a})

instance FromXML OrganizationsDecisionDetail where
  parseXML x =
    OrganizationsDecisionDetail' <$> (x .@? "AllowedByOrganizations")

instance Hashable OrganizationsDecisionDetail

instance NFData OrganizationsDecisionDetail
