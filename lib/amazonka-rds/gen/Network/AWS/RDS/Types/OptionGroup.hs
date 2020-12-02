{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.OptionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.OptionGroup where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.Option

-- |
--
--
--
-- /See:/ 'optionGroup' smart constructor.
data OptionGroup = OptionGroup'
  { _ogOptionGroupDescription ::
      !(Maybe Text),
    _ogVPCId :: !(Maybe Text),
    _ogAllowsVPCAndNonVPCInstanceMemberships :: !(Maybe Bool),
    _ogEngineName :: !(Maybe Text),
    _ogOptionGroupARN :: !(Maybe Text),
    _ogMajorEngineVersion :: !(Maybe Text),
    _ogOptions :: !(Maybe [Option]),
    _ogOptionGroupName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OptionGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogOptionGroupDescription' - Provides a description of the option group.
--
-- * 'ogVPCId' - If __AllowsVpcAndNonVpcInstanceMemberships__ is @false@ , this field is blank. If __AllowsVpcAndNonVpcInstanceMemberships__ is @true@ and this field is blank, then this option group can be applied to both VPC and non-VPC instances. If this field contains a value, then this option group can only be applied to instances that are in the VPC indicated by this field.
--
-- * 'ogAllowsVPCAndNonVPCInstanceMemberships' - Indicates whether this option group can be applied to both VPC and non-VPC instances. The value @true@ indicates the option group can be applied to both VPC and non-VPC instances.
--
-- * 'ogEngineName' - Indicates the name of the engine that this option group can be applied to.
--
-- * 'ogOptionGroupARN' - The Amazon Resource Name (ARN) for the option group.
--
-- * 'ogMajorEngineVersion' - Indicates the major engine version associated with this option group.
--
-- * 'ogOptions' - Indicates what options are available in the option group.
--
-- * 'ogOptionGroupName' - Specifies the name of the option group.
optionGroup ::
  OptionGroup
optionGroup =
  OptionGroup'
    { _ogOptionGroupDescription = Nothing,
      _ogVPCId = Nothing,
      _ogAllowsVPCAndNonVPCInstanceMemberships = Nothing,
      _ogEngineName = Nothing,
      _ogOptionGroupARN = Nothing,
      _ogMajorEngineVersion = Nothing,
      _ogOptions = Nothing,
      _ogOptionGroupName = Nothing
    }

-- | Provides a description of the option group.
ogOptionGroupDescription :: Lens' OptionGroup (Maybe Text)
ogOptionGroupDescription = lens _ogOptionGroupDescription (\s a -> s {_ogOptionGroupDescription = a})

-- | If __AllowsVpcAndNonVpcInstanceMemberships__ is @false@ , this field is blank. If __AllowsVpcAndNonVpcInstanceMemberships__ is @true@ and this field is blank, then this option group can be applied to both VPC and non-VPC instances. If this field contains a value, then this option group can only be applied to instances that are in the VPC indicated by this field.
ogVPCId :: Lens' OptionGroup (Maybe Text)
ogVPCId = lens _ogVPCId (\s a -> s {_ogVPCId = a})

-- | Indicates whether this option group can be applied to both VPC and non-VPC instances. The value @true@ indicates the option group can be applied to both VPC and non-VPC instances.
ogAllowsVPCAndNonVPCInstanceMemberships :: Lens' OptionGroup (Maybe Bool)
ogAllowsVPCAndNonVPCInstanceMemberships = lens _ogAllowsVPCAndNonVPCInstanceMemberships (\s a -> s {_ogAllowsVPCAndNonVPCInstanceMemberships = a})

-- | Indicates the name of the engine that this option group can be applied to.
ogEngineName :: Lens' OptionGroup (Maybe Text)
ogEngineName = lens _ogEngineName (\s a -> s {_ogEngineName = a})

-- | The Amazon Resource Name (ARN) for the option group.
ogOptionGroupARN :: Lens' OptionGroup (Maybe Text)
ogOptionGroupARN = lens _ogOptionGroupARN (\s a -> s {_ogOptionGroupARN = a})

-- | Indicates the major engine version associated with this option group.
ogMajorEngineVersion :: Lens' OptionGroup (Maybe Text)
ogMajorEngineVersion = lens _ogMajorEngineVersion (\s a -> s {_ogMajorEngineVersion = a})

-- | Indicates what options are available in the option group.
ogOptions :: Lens' OptionGroup [Option]
ogOptions = lens _ogOptions (\s a -> s {_ogOptions = a}) . _Default . _Coerce

-- | Specifies the name of the option group.
ogOptionGroupName :: Lens' OptionGroup (Maybe Text)
ogOptionGroupName = lens _ogOptionGroupName (\s a -> s {_ogOptionGroupName = a})

instance FromXML OptionGroup where
  parseXML x =
    OptionGroup'
      <$> (x .@? "OptionGroupDescription")
      <*> (x .@? "VpcId")
      <*> (x .@? "AllowsVpcAndNonVpcInstanceMemberships")
      <*> (x .@? "EngineName")
      <*> (x .@? "OptionGroupArn")
      <*> (x .@? "MajorEngineVersion")
      <*> (x .@? "Options" .!@ mempty >>= may (parseXMLList "Option"))
      <*> (x .@? "OptionGroupName")

instance Hashable OptionGroup

instance NFData OptionGroup
