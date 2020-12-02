{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.DomainDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.DomainDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the domain.
--
--
--
-- /See:/ 'domainDetails' smart constructor.
newtype DomainDetails = DomainDetails' {_ddDomain :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DomainDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddDomain' - The domain information for the AWS API call.
domainDetails ::
  DomainDetails
domainDetails = DomainDetails' {_ddDomain = Nothing}

-- | The domain information for the AWS API call.
ddDomain :: Lens' DomainDetails (Maybe Text)
ddDomain = lens _ddDomain (\s a -> s {_ddDomain = a})

instance FromJSON DomainDetails where
  parseJSON =
    withObject
      "DomainDetails"
      (\x -> DomainDetails' <$> (x .:? "domain"))

instance Hashable DomainDetails

instance NFData DomainDetails
