{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.ResourceAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.ResourceAttribute where

import Network.AWS.Lens
import Network.AWS.MigrationHub.Types.ResourceAttributeType
import Network.AWS.Prelude

-- | Attribute associated with a resource.
--
--
-- Note the corresponding format required per type listed below:
--
--     * IPV4    * @x.x.x.x@
--
-- /where x is an integer in the range [0,255]/
--
--     * IPV6    * @y : y : y : y : y : y : y : y@
--
-- /where y is a hexadecimal between 0 and FFFF. [0, FFFF]/
--
--     * MAC_ADDRESS    * @^([0-9A-Fa-f]{2}[:-]){5}([0-9A-Fa-f]{2})$@
--
--     * FQDN    * @^[^<>{}\\\\/?,=\\p{Cntrl}]{1,256}$@
--
--
--
--
-- /See:/ 'resourceAttribute' smart constructor.
data ResourceAttribute = ResourceAttribute'
  { _raType ::
      !ResourceAttributeType,
    _raValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raType' - Type of resource.
--
-- * 'raValue' - Value of the resource type.
resourceAttribute ::
  -- | 'raType'
  ResourceAttributeType ->
  -- | 'raValue'
  Text ->
  ResourceAttribute
resourceAttribute pType_ pValue_ =
  ResourceAttribute' {_raType = pType_, _raValue = pValue_}

-- | Type of resource.
raType :: Lens' ResourceAttribute ResourceAttributeType
raType = lens _raType (\s a -> s {_raType = a})

-- | Value of the resource type.
raValue :: Lens' ResourceAttribute Text
raValue = lens _raValue (\s a -> s {_raValue = a})

instance FromJSON ResourceAttribute where
  parseJSON =
    withObject
      "ResourceAttribute"
      (\x -> ResourceAttribute' <$> (x .: "Type") <*> (x .: "Value"))

instance Hashable ResourceAttribute

instance NFData ResourceAttribute

instance ToJSON ResourceAttribute where
  toJSON ResourceAttribute' {..} =
    object
      (catMaybes [Just ("Type" .= _raType), Just ("Value" .= _raValue)])
