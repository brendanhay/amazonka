{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.AutoDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.AutoDeployment where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organization or organizational unit (OU).
--
--
--
-- /See:/ 'autoDeployment' smart constructor.
data AutoDeployment = AutoDeployment'
  { _adEnabled :: !(Maybe Bool),
    _adRetainStacksOnAccountRemoval :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoDeployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adEnabled' - If set to @true@ , StackSets automatically deploys additional stack instances to AWS Organizations accounts that are added to a target organization or organizational unit (OU) in the specified Regions. If an account is removed from a target organization or OU, StackSets deletes stack instances from the account in the specified Regions.
--
-- * 'adRetainStacksOnAccountRemoval' - If set to @true@ , stack resources are retained when an account is removed from a target organization or OU. If set to @false@ , stack resources are deleted. Specify only if @Enabled@ is set to @True@ .
autoDeployment ::
  AutoDeployment
autoDeployment =
  AutoDeployment'
    { _adEnabled = Nothing,
      _adRetainStacksOnAccountRemoval = Nothing
    }

-- | If set to @true@ , StackSets automatically deploys additional stack instances to AWS Organizations accounts that are added to a target organization or organizational unit (OU) in the specified Regions. If an account is removed from a target organization or OU, StackSets deletes stack instances from the account in the specified Regions.
adEnabled :: Lens' AutoDeployment (Maybe Bool)
adEnabled = lens _adEnabled (\s a -> s {_adEnabled = a})

-- | If set to @true@ , stack resources are retained when an account is removed from a target organization or OU. If set to @false@ , stack resources are deleted. Specify only if @Enabled@ is set to @True@ .
adRetainStacksOnAccountRemoval :: Lens' AutoDeployment (Maybe Bool)
adRetainStacksOnAccountRemoval = lens _adRetainStacksOnAccountRemoval (\s a -> s {_adRetainStacksOnAccountRemoval = a})

instance FromXML AutoDeployment where
  parseXML x =
    AutoDeployment'
      <$> (x .@? "Enabled") <*> (x .@? "RetainStacksOnAccountRemoval")

instance Hashable AutoDeployment

instance NFData AutoDeployment

instance ToQuery AutoDeployment where
  toQuery AutoDeployment' {..} =
    mconcat
      [ "Enabled" =: _adEnabled,
        "RetainStacksOnAccountRemoval" =: _adRetainStacksOnAccountRemoval
      ]
