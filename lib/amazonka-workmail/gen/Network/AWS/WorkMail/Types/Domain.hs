{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.Domain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.Domain where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The domain to associate with an Amazon WorkMail organization.
--
--
-- When you configure a domain hosted in Amazon Route 53 (Route 53), all recommended DNS records are added to the organization when you create it. For more information, see <https://docs.aws.amazon.com/workmail/latest/adminguide/add_domain.html Adding a domain> in the /Amazon WorkMail Administrator Guide/ .
--
--
-- /See:/ 'domain' smart constructor.
data Domain = Domain'
  { _dHostedZoneId :: !(Maybe Text),
    _dDomainName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Domain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dHostedZoneId' - The hosted zone ID for a domain hosted in Route 53. Required when configuring a domain hosted in Route 53.
--
-- * 'dDomainName' - The fully qualified domain name.
domain ::
  Domain
domain = Domain' {_dHostedZoneId = Nothing, _dDomainName = Nothing}

-- | The hosted zone ID for a domain hosted in Route 53. Required when configuring a domain hosted in Route 53.
dHostedZoneId :: Lens' Domain (Maybe Text)
dHostedZoneId = lens _dHostedZoneId (\s a -> s {_dHostedZoneId = a})

-- | The fully qualified domain name.
dDomainName :: Lens' Domain (Maybe Text)
dDomainName = lens _dDomainName (\s a -> s {_dDomainName = a})

instance Hashable Domain

instance NFData Domain

instance ToJSON Domain where
  toJSON Domain' {..} =
    object
      ( catMaybes
          [ ("HostedZoneId" .=) <$> _dHostedZoneId,
            ("DomainName" .=) <$> _dDomainName
          ]
      )
