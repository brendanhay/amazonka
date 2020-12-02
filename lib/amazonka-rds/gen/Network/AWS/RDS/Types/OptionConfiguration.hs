{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.OptionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.OptionConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.OptionSetting

-- | A list of all available options
--
--
--
-- /See:/ 'optionConfiguration' smart constructor.
data OptionConfiguration = OptionConfiguration'
  { _ocOptionSettings ::
      !(Maybe [OptionSetting]),
    _ocVPCSecurityGroupMemberships :: !(Maybe [Text]),
    _ocDBSecurityGroupMemberships :: !(Maybe [Text]),
    _ocOptionVersion :: !(Maybe Text),
    _ocPort :: !(Maybe Int),
    _ocOptionName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OptionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocOptionSettings' - The option settings to include in an option group.
--
-- * 'ocVPCSecurityGroupMemberships' - A list of VpcSecurityGroupMembership name strings used for this option.
--
-- * 'ocDBSecurityGroupMemberships' - A list of DBSecurityGroupMembership name strings used for this option.
--
-- * 'ocOptionVersion' - The version for the option.
--
-- * 'ocPort' - The optional port for the option.
--
-- * 'ocOptionName' - The configuration of options to include in a group.
optionConfiguration ::
  -- | 'ocOptionName'
  Text ->
  OptionConfiguration
optionConfiguration pOptionName_ =
  OptionConfiguration'
    { _ocOptionSettings = Nothing,
      _ocVPCSecurityGroupMemberships = Nothing,
      _ocDBSecurityGroupMemberships = Nothing,
      _ocOptionVersion = Nothing,
      _ocPort = Nothing,
      _ocOptionName = pOptionName_
    }

-- | The option settings to include in an option group.
ocOptionSettings :: Lens' OptionConfiguration [OptionSetting]
ocOptionSettings = lens _ocOptionSettings (\s a -> s {_ocOptionSettings = a}) . _Default . _Coerce

-- | A list of VpcSecurityGroupMembership name strings used for this option.
ocVPCSecurityGroupMemberships :: Lens' OptionConfiguration [Text]
ocVPCSecurityGroupMemberships = lens _ocVPCSecurityGroupMemberships (\s a -> s {_ocVPCSecurityGroupMemberships = a}) . _Default . _Coerce

-- | A list of DBSecurityGroupMembership name strings used for this option.
ocDBSecurityGroupMemberships :: Lens' OptionConfiguration [Text]
ocDBSecurityGroupMemberships = lens _ocDBSecurityGroupMemberships (\s a -> s {_ocDBSecurityGroupMemberships = a}) . _Default . _Coerce

-- | The version for the option.
ocOptionVersion :: Lens' OptionConfiguration (Maybe Text)
ocOptionVersion = lens _ocOptionVersion (\s a -> s {_ocOptionVersion = a})

-- | The optional port for the option.
ocPort :: Lens' OptionConfiguration (Maybe Int)
ocPort = lens _ocPort (\s a -> s {_ocPort = a})

-- | The configuration of options to include in a group.
ocOptionName :: Lens' OptionConfiguration Text
ocOptionName = lens _ocOptionName (\s a -> s {_ocOptionName = a})

instance Hashable OptionConfiguration

instance NFData OptionConfiguration

instance ToQuery OptionConfiguration where
  toQuery OptionConfiguration' {..} =
    mconcat
      [ "OptionSettings"
          =: toQuery (toQueryList "OptionSetting" <$> _ocOptionSettings),
        "VpcSecurityGroupMemberships"
          =: toQuery
            ( toQueryList "VpcSecurityGroupId"
                <$> _ocVPCSecurityGroupMemberships
            ),
        "DBSecurityGroupMemberships"
          =: toQuery
            ( toQueryList "DBSecurityGroupName"
                <$> _ocDBSecurityGroupMemberships
            ),
        "OptionVersion" =: _ocOptionVersion,
        "Port" =: _ocPort,
        "OptionName" =: _ocOptionName
      ]
