{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.Parameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.Parameter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.ParameterType

-- | An Systems Manager parameter in Parameter Store.
--
--
--
-- /See:/ 'parameter' smart constructor.
data Parameter = Parameter'
  { _pLastModifiedDate :: !(Maybe POSIX),
    _pSelector :: !(Maybe Text),
    _pARN :: !(Maybe Text),
    _pValue :: !(Maybe Text),
    _pSourceResult :: !(Maybe Text),
    _pName :: !(Maybe Text),
    _pVersion :: !(Maybe Integer),
    _pType :: !(Maybe ParameterType),
    _pDataType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Parameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pLastModifiedDate' - Date the parameter was last changed or updated and the parameter version was created.
--
-- * 'pSelector' - Either the version number or the label used to retrieve the parameter value. Specify selectors by using one of the following formats: parameter_name:version parameter_name:label
--
-- * 'pARN' - The Amazon Resource Name (ARN) of the parameter.
--
-- * 'pValue' - The parameter value.
--
-- * 'pSourceResult' - Applies to parameters that reference information in other AWS services. SourceResult is the raw result or response from the source.
--
-- * 'pName' - The name of the parameter.
--
-- * 'pVersion' - The parameter version.
--
-- * 'pType' - The type of parameter. Valid values include the following: @String@ , @StringList@ , and @SecureString@ .
--
-- * 'pDataType' - The data type of the parameter, such as @text@ or @aws:ec2:image@ . The default is @text@ .
parameter ::
  Parameter
parameter =
  Parameter'
    { _pLastModifiedDate = Nothing,
      _pSelector = Nothing,
      _pARN = Nothing,
      _pValue = Nothing,
      _pSourceResult = Nothing,
      _pName = Nothing,
      _pVersion = Nothing,
      _pType = Nothing,
      _pDataType = Nothing
    }

-- | Date the parameter was last changed or updated and the parameter version was created.
pLastModifiedDate :: Lens' Parameter (Maybe UTCTime)
pLastModifiedDate = lens _pLastModifiedDate (\s a -> s {_pLastModifiedDate = a}) . mapping _Time

-- | Either the version number or the label used to retrieve the parameter value. Specify selectors by using one of the following formats: parameter_name:version parameter_name:label
pSelector :: Lens' Parameter (Maybe Text)
pSelector = lens _pSelector (\s a -> s {_pSelector = a})

-- | The Amazon Resource Name (ARN) of the parameter.
pARN :: Lens' Parameter (Maybe Text)
pARN = lens _pARN (\s a -> s {_pARN = a})

-- | The parameter value.
pValue :: Lens' Parameter (Maybe Text)
pValue = lens _pValue (\s a -> s {_pValue = a})

-- | Applies to parameters that reference information in other AWS services. SourceResult is the raw result or response from the source.
pSourceResult :: Lens' Parameter (Maybe Text)
pSourceResult = lens _pSourceResult (\s a -> s {_pSourceResult = a})

-- | The name of the parameter.
pName :: Lens' Parameter (Maybe Text)
pName = lens _pName (\s a -> s {_pName = a})

-- | The parameter version.
pVersion :: Lens' Parameter (Maybe Integer)
pVersion = lens _pVersion (\s a -> s {_pVersion = a})

-- | The type of parameter. Valid values include the following: @String@ , @StringList@ , and @SecureString@ .
pType :: Lens' Parameter (Maybe ParameterType)
pType = lens _pType (\s a -> s {_pType = a})

-- | The data type of the parameter, such as @text@ or @aws:ec2:image@ . The default is @text@ .
pDataType :: Lens' Parameter (Maybe Text)
pDataType = lens _pDataType (\s a -> s {_pDataType = a})

instance FromJSON Parameter where
  parseJSON =
    withObject
      "Parameter"
      ( \x ->
          Parameter'
            <$> (x .:? "LastModifiedDate")
            <*> (x .:? "Selector")
            <*> (x .:? "ARN")
            <*> (x .:? "Value")
            <*> (x .:? "SourceResult")
            <*> (x .:? "Name")
            <*> (x .:? "Version")
            <*> (x .:? "Type")
            <*> (x .:? "DataType")
      )

instance Hashable Parameter

instance NFData Parameter
