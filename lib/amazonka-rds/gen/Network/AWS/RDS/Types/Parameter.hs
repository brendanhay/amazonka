{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Parameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Parameter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.ApplyMethod

-- | This data type is used as a request parameter in the @ModifyDBParameterGroup@ and @ResetDBParameterGroup@ actions.
--
--
-- This data type is used as a response element in the @DescribeEngineDefaultParameters@ and @DescribeDBParameters@ actions.
--
--
-- /See:/ 'parameter' smart constructor.
data Parameter = Parameter'
  { _pApplyType :: !(Maybe Text),
    _pParameterValue :: !(Maybe Text),
    _pSupportedEngineModes :: !(Maybe [Text]),
    _pApplyMethod :: !(Maybe ApplyMethod),
    _pMinimumEngineVersion :: !(Maybe Text),
    _pSource :: !(Maybe Text),
    _pIsModifiable :: !(Maybe Bool),
    _pDataType :: !(Maybe Text),
    _pAllowedValues :: !(Maybe Text),
    _pParameterName :: !(Maybe Text),
    _pDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Parameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pApplyType' - Specifies the engine specific parameters type.
--
-- * 'pParameterValue' - Specifies the value of the parameter.
--
-- * 'pSupportedEngineModes' - The valid DB engine modes.
--
-- * 'pApplyMethod' - Indicates when to apply parameter updates.
--
-- * 'pMinimumEngineVersion' - The earliest engine version to which the parameter can apply.
--
-- * 'pSource' - Indicates the source of the parameter value.
--
-- * 'pIsModifiable' - Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
--
-- * 'pDataType' - Specifies the valid data type for the parameter.
--
-- * 'pAllowedValues' - Specifies the valid range of values for the parameter.
--
-- * 'pParameterName' - Specifies the name of the parameter.
--
-- * 'pDescription' - Provides a description of the parameter.
parameter ::
  Parameter
parameter =
  Parameter'
    { _pApplyType = Nothing,
      _pParameterValue = Nothing,
      _pSupportedEngineModes = Nothing,
      _pApplyMethod = Nothing,
      _pMinimumEngineVersion = Nothing,
      _pSource = Nothing,
      _pIsModifiable = Nothing,
      _pDataType = Nothing,
      _pAllowedValues = Nothing,
      _pParameterName = Nothing,
      _pDescription = Nothing
    }

-- | Specifies the engine specific parameters type.
pApplyType :: Lens' Parameter (Maybe Text)
pApplyType = lens _pApplyType (\s a -> s {_pApplyType = a})

-- | Specifies the value of the parameter.
pParameterValue :: Lens' Parameter (Maybe Text)
pParameterValue = lens _pParameterValue (\s a -> s {_pParameterValue = a})

-- | The valid DB engine modes.
pSupportedEngineModes :: Lens' Parameter [Text]
pSupportedEngineModes = lens _pSupportedEngineModes (\s a -> s {_pSupportedEngineModes = a}) . _Default . _Coerce

-- | Indicates when to apply parameter updates.
pApplyMethod :: Lens' Parameter (Maybe ApplyMethod)
pApplyMethod = lens _pApplyMethod (\s a -> s {_pApplyMethod = a})

-- | The earliest engine version to which the parameter can apply.
pMinimumEngineVersion :: Lens' Parameter (Maybe Text)
pMinimumEngineVersion = lens _pMinimumEngineVersion (\s a -> s {_pMinimumEngineVersion = a})

-- | Indicates the source of the parameter value.
pSource :: Lens' Parameter (Maybe Text)
pSource = lens _pSource (\s a -> s {_pSource = a})

-- | Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
pIsModifiable :: Lens' Parameter (Maybe Bool)
pIsModifiable = lens _pIsModifiable (\s a -> s {_pIsModifiable = a})

-- | Specifies the valid data type for the parameter.
pDataType :: Lens' Parameter (Maybe Text)
pDataType = lens _pDataType (\s a -> s {_pDataType = a})

-- | Specifies the valid range of values for the parameter.
pAllowedValues :: Lens' Parameter (Maybe Text)
pAllowedValues = lens _pAllowedValues (\s a -> s {_pAllowedValues = a})

-- | Specifies the name of the parameter.
pParameterName :: Lens' Parameter (Maybe Text)
pParameterName = lens _pParameterName (\s a -> s {_pParameterName = a})

-- | Provides a description of the parameter.
pDescription :: Lens' Parameter (Maybe Text)
pDescription = lens _pDescription (\s a -> s {_pDescription = a})

instance FromXML Parameter where
  parseXML x =
    Parameter'
      <$> (x .@? "ApplyType")
      <*> (x .@? "ParameterValue")
      <*> ( x .@? "SupportedEngineModes" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@? "ApplyMethod")
      <*> (x .@? "MinimumEngineVersion")
      <*> (x .@? "Source")
      <*> (x .@? "IsModifiable")
      <*> (x .@? "DataType")
      <*> (x .@? "AllowedValues")
      <*> (x .@? "ParameterName")
      <*> (x .@? "Description")

instance Hashable Parameter

instance NFData Parameter

instance ToQuery Parameter where
  toQuery Parameter' {..} =
    mconcat
      [ "ApplyType" =: _pApplyType,
        "ParameterValue" =: _pParameterValue,
        "SupportedEngineModes"
          =: toQuery (toQueryList "member" <$> _pSupportedEngineModes),
        "ApplyMethod" =: _pApplyMethod,
        "MinimumEngineVersion" =: _pMinimumEngineVersion,
        "Source" =: _pSource,
        "IsModifiable" =: _pIsModifiable,
        "DataType" =: _pDataType,
        "AllowedValues" =: _pAllowedValues,
        "ParameterName" =: _pParameterName,
        "Description" =: _pDescription
      ]
