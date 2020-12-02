{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DomainEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DomainEntry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a domain recordset entry.
--
--
--
-- /See:/ 'domainEntry' smart constructor.
data DomainEntry = DomainEntry'
  { _deIsAlias :: !(Maybe Bool),
    _deName :: !(Maybe Text),
    _deId :: !(Maybe Text),
    _deOptions :: !(Maybe (Map Text (Text))),
    _deType :: !(Maybe Text),
    _deTarget :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DomainEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deIsAlias' - When @true@ , specifies whether the domain entry is an alias used by the Lightsail load balancer. You can include an alias (A type) record in your request, which points to a load balancer DNS name and routes traffic to your load balancer.
--
-- * 'deName' - The name of the domain.
--
-- * 'deId' - The ID of the domain recordset entry.
--
-- * 'deOptions' - (Deprecated) The options for the domain entry.
--
-- * 'deType' - The type of domain entry, such as address (A), canonical name (CNAME), mail exchanger (MX), name server (NS), start of authority (SOA), service locator (SRV), or text (TXT). The following domain entry types can be used:     * @A@      * @CNAME@      * @MX@      * @NS@      * @SOA@      * @SRV@      * @TXT@
--
-- * 'deTarget' - The target AWS name server (e.g., @ns-111.awsdns-22.com.@ ). For Lightsail load balancers, the value looks like @ab1234c56789c6b86aba6fb203d443bc-123456789.us-east-2.elb.amazonaws.com@ . Be sure to also set @isAlias@ to @true@ when setting up an A record for a load balancer.
domainEntry ::
  DomainEntry
domainEntry =
  DomainEntry'
    { _deIsAlias = Nothing,
      _deName = Nothing,
      _deId = Nothing,
      _deOptions = Nothing,
      _deType = Nothing,
      _deTarget = Nothing
    }

-- | When @true@ , specifies whether the domain entry is an alias used by the Lightsail load balancer. You can include an alias (A type) record in your request, which points to a load balancer DNS name and routes traffic to your load balancer.
deIsAlias :: Lens' DomainEntry (Maybe Bool)
deIsAlias = lens _deIsAlias (\s a -> s {_deIsAlias = a})

-- | The name of the domain.
deName :: Lens' DomainEntry (Maybe Text)
deName = lens _deName (\s a -> s {_deName = a})

-- | The ID of the domain recordset entry.
deId :: Lens' DomainEntry (Maybe Text)
deId = lens _deId (\s a -> s {_deId = a})

-- | (Deprecated) The options for the domain entry.
deOptions :: Lens' DomainEntry (HashMap Text (Text))
deOptions = lens _deOptions (\s a -> s {_deOptions = a}) . _Default . _Map

-- | The type of domain entry, such as address (A), canonical name (CNAME), mail exchanger (MX), name server (NS), start of authority (SOA), service locator (SRV), or text (TXT). The following domain entry types can be used:     * @A@      * @CNAME@      * @MX@      * @NS@      * @SOA@      * @SRV@      * @TXT@
deType :: Lens' DomainEntry (Maybe Text)
deType = lens _deType (\s a -> s {_deType = a})

-- | The target AWS name server (e.g., @ns-111.awsdns-22.com.@ ). For Lightsail load balancers, the value looks like @ab1234c56789c6b86aba6fb203d443bc-123456789.us-east-2.elb.amazonaws.com@ . Be sure to also set @isAlias@ to @true@ when setting up an A record for a load balancer.
deTarget :: Lens' DomainEntry (Maybe Text)
deTarget = lens _deTarget (\s a -> s {_deTarget = a})

instance FromJSON DomainEntry where
  parseJSON =
    withObject
      "DomainEntry"
      ( \x ->
          DomainEntry'
            <$> (x .:? "isAlias")
            <*> (x .:? "name")
            <*> (x .:? "id")
            <*> (x .:? "options" .!= mempty)
            <*> (x .:? "type")
            <*> (x .:? "target")
      )

instance Hashable DomainEntry

instance NFData DomainEntry

instance ToJSON DomainEntry where
  toJSON DomainEntry' {..} =
    object
      ( catMaybes
          [ ("isAlias" .=) <$> _deIsAlias,
            ("name" .=) <$> _deName,
            ("id" .=) <$> _deId,
            ("options" .=) <$> _deOptions,
            ("type" .=) <$> _deType,
            ("target" .=) <$> _deTarget
          ]
      )
