{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.DomainInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.DomainInfo where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.RegistrationStatus

-- | Contains general information about a domain.
--
--
--
-- /See:/ 'domainInfo' smart constructor.
data DomainInfo = DomainInfo'
  { _diArn :: !(Maybe Text),
    _diDescription :: !(Maybe Text),
    _diName :: !Text,
    _diStatus :: !RegistrationStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DomainInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diArn' - The ARN of the domain.
--
-- * 'diDescription' - The description of the domain provided through 'RegisterDomain' .
--
-- * 'diName' - The name of the domain. This name is unique within the account.
--
-- * 'diStatus' - The status of the domain:     * @REGISTERED@ – The domain is properly registered and available. You can use this domain for registering types and creating new workflow executions.      * @DEPRECATED@ – The domain was deprecated using 'DeprecateDomain' , but is still in use. You should not create new workflow executions in this domain.
domainInfo ::
  -- | 'diName'
  Text ->
  -- | 'diStatus'
  RegistrationStatus ->
  DomainInfo
domainInfo pName_ pStatus_ =
  DomainInfo'
    { _diArn = Nothing,
      _diDescription = Nothing,
      _diName = pName_,
      _diStatus = pStatus_
    }

-- | The ARN of the domain.
diArn :: Lens' DomainInfo (Maybe Text)
diArn = lens _diArn (\s a -> s {_diArn = a})

-- | The description of the domain provided through 'RegisterDomain' .
diDescription :: Lens' DomainInfo (Maybe Text)
diDescription = lens _diDescription (\s a -> s {_diDescription = a})

-- | The name of the domain. This name is unique within the account.
diName :: Lens' DomainInfo Text
diName = lens _diName (\s a -> s {_diName = a})

-- | The status of the domain:     * @REGISTERED@ – The domain is properly registered and available. You can use this domain for registering types and creating new workflow executions.      * @DEPRECATED@ – The domain was deprecated using 'DeprecateDomain' , but is still in use. You should not create new workflow executions in this domain.
diStatus :: Lens' DomainInfo RegistrationStatus
diStatus = lens _diStatus (\s a -> s {_diStatus = a})

instance FromJSON DomainInfo where
  parseJSON =
    withObject
      "DomainInfo"
      ( \x ->
          DomainInfo'
            <$> (x .:? "arn")
            <*> (x .:? "description")
            <*> (x .: "name")
            <*> (x .: "status")
      )

instance Hashable DomainInfo

instance NFData DomainInfo
