{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ParameterHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ParameterHistory where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.ParameterInlinePolicy
import Network.AWS.SSM.Types.ParameterTier
import Network.AWS.SSM.Types.ParameterType

-- | Information about parameter usage.
--
--
--
-- /See:/ 'parameterHistory' smart constructor.
data ParameterHistory = ParameterHistory'
  { _phLastModifiedDate ::
      !(Maybe POSIX),
    _phKeyId :: !(Maybe Text),
    _phValue :: !(Maybe Text),
    _phName :: !(Maybe Text),
    _phTier :: !(Maybe ParameterTier),
    _phVersion :: !(Maybe Integer),
    _phLastModifiedUser :: !(Maybe Text),
    _phLabels :: !(Maybe (List1 Text)),
    _phAllowedPattern :: !(Maybe Text),
    _phType :: !(Maybe ParameterType),
    _phDataType :: !(Maybe Text),
    _phDescription :: !(Maybe Text),
    _phPolicies :: !(Maybe [ParameterInlinePolicy])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ParameterHistory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'phLastModifiedDate' - Date the parameter was last changed or updated.
--
-- * 'phKeyId' - The ID of the query key used for this parameter.
--
-- * 'phValue' - The parameter value.
--
-- * 'phName' - The name of the parameter.
--
-- * 'phTier' - The parameter tier.
--
-- * 'phVersion' - The parameter version.
--
-- * 'phLastModifiedUser' - Amazon Resource Name (ARN) of the AWS user who last changed the parameter.
--
-- * 'phLabels' - Labels assigned to the parameter version.
--
-- * 'phAllowedPattern' - Parameter names can include the following letters and symbols. a-zA-Z0-9_.-
--
-- * 'phType' - The type of parameter used.
--
-- * 'phDataType' - The data type of the parameter, such as @text@ or @aws:ec2:image@ . The default is @text@ .
--
-- * 'phDescription' - Information about the parameter.
--
-- * 'phPolicies' - Information about the policies assigned to a parameter. <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-policies.html Assigning parameter policies> in the /AWS Systems Manager User Guide/ .
parameterHistory ::
  ParameterHistory
parameterHistory =
  ParameterHistory'
    { _phLastModifiedDate = Nothing,
      _phKeyId = Nothing,
      _phValue = Nothing,
      _phName = Nothing,
      _phTier = Nothing,
      _phVersion = Nothing,
      _phLastModifiedUser = Nothing,
      _phLabels = Nothing,
      _phAllowedPattern = Nothing,
      _phType = Nothing,
      _phDataType = Nothing,
      _phDescription = Nothing,
      _phPolicies = Nothing
    }

-- | Date the parameter was last changed or updated.
phLastModifiedDate :: Lens' ParameterHistory (Maybe UTCTime)
phLastModifiedDate = lens _phLastModifiedDate (\s a -> s {_phLastModifiedDate = a}) . mapping _Time

-- | The ID of the query key used for this parameter.
phKeyId :: Lens' ParameterHistory (Maybe Text)
phKeyId = lens _phKeyId (\s a -> s {_phKeyId = a})

-- | The parameter value.
phValue :: Lens' ParameterHistory (Maybe Text)
phValue = lens _phValue (\s a -> s {_phValue = a})

-- | The name of the parameter.
phName :: Lens' ParameterHistory (Maybe Text)
phName = lens _phName (\s a -> s {_phName = a})

-- | The parameter tier.
phTier :: Lens' ParameterHistory (Maybe ParameterTier)
phTier = lens _phTier (\s a -> s {_phTier = a})

-- | The parameter version.
phVersion :: Lens' ParameterHistory (Maybe Integer)
phVersion = lens _phVersion (\s a -> s {_phVersion = a})

-- | Amazon Resource Name (ARN) of the AWS user who last changed the parameter.
phLastModifiedUser :: Lens' ParameterHistory (Maybe Text)
phLastModifiedUser = lens _phLastModifiedUser (\s a -> s {_phLastModifiedUser = a})

-- | Labels assigned to the parameter version.
phLabels :: Lens' ParameterHistory (Maybe (NonEmpty Text))
phLabels = lens _phLabels (\s a -> s {_phLabels = a}) . mapping _List1

-- | Parameter names can include the following letters and symbols. a-zA-Z0-9_.-
phAllowedPattern :: Lens' ParameterHistory (Maybe Text)
phAllowedPattern = lens _phAllowedPattern (\s a -> s {_phAllowedPattern = a})

-- | The type of parameter used.
phType :: Lens' ParameterHistory (Maybe ParameterType)
phType = lens _phType (\s a -> s {_phType = a})

-- | The data type of the parameter, such as @text@ or @aws:ec2:image@ . The default is @text@ .
phDataType :: Lens' ParameterHistory (Maybe Text)
phDataType = lens _phDataType (\s a -> s {_phDataType = a})

-- | Information about the parameter.
phDescription :: Lens' ParameterHistory (Maybe Text)
phDescription = lens _phDescription (\s a -> s {_phDescription = a})

-- | Information about the policies assigned to a parameter. <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-policies.html Assigning parameter policies> in the /AWS Systems Manager User Guide/ .
phPolicies :: Lens' ParameterHistory [ParameterInlinePolicy]
phPolicies = lens _phPolicies (\s a -> s {_phPolicies = a}) . _Default . _Coerce

instance FromJSON ParameterHistory where
  parseJSON =
    withObject
      "ParameterHistory"
      ( \x ->
          ParameterHistory'
            <$> (x .:? "LastModifiedDate")
            <*> (x .:? "KeyId")
            <*> (x .:? "Value")
            <*> (x .:? "Name")
            <*> (x .:? "Tier")
            <*> (x .:? "Version")
            <*> (x .:? "LastModifiedUser")
            <*> (x .:? "Labels")
            <*> (x .:? "AllowedPattern")
            <*> (x .:? "Type")
            <*> (x .:? "DataType")
            <*> (x .:? "Description")
            <*> (x .:? "Policies" .!= mempty)
      )

instance Hashable ParameterHistory

instance NFData ParameterHistory
