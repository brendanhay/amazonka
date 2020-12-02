{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.TypeSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.TypeSummary where

import Network.AWS.CloudFormation.Types.RegistryType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains summary information about the specified CloudFormation type.
--
--
--
-- /See:/ 'typeSummary' smart constructor.
data TypeSummary = TypeSummary'
  { _tsLastUpdated :: !(Maybe ISO8601),
    _tsTypeName :: !(Maybe Text),
    _tsDefaultVersionId :: !(Maybe Text),
    _tsTypeARN :: !(Maybe Text),
    _tsType :: !(Maybe RegistryType),
    _tsDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TypeSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsLastUpdated' - When the current default version of the type was registered.
--
-- * 'tsTypeName' - The name of the type.
--
-- * 'tsDefaultVersionId' - The ID of the default version of the type. The default version is used when the type version is not specified. To set the default version of a type, use @'SetTypeDefaultVersion' @ .
--
-- * 'tsTypeARN' - The Amazon Resource Name (ARN) of the type.
--
-- * 'tsType' - The kind of type.
--
-- * 'tsDescription' - The description of the type.
typeSummary ::
  TypeSummary
typeSummary =
  TypeSummary'
    { _tsLastUpdated = Nothing,
      _tsTypeName = Nothing,
      _tsDefaultVersionId = Nothing,
      _tsTypeARN = Nothing,
      _tsType = Nothing,
      _tsDescription = Nothing
    }

-- | When the current default version of the type was registered.
tsLastUpdated :: Lens' TypeSummary (Maybe UTCTime)
tsLastUpdated = lens _tsLastUpdated (\s a -> s {_tsLastUpdated = a}) . mapping _Time

-- | The name of the type.
tsTypeName :: Lens' TypeSummary (Maybe Text)
tsTypeName = lens _tsTypeName (\s a -> s {_tsTypeName = a})

-- | The ID of the default version of the type. The default version is used when the type version is not specified. To set the default version of a type, use @'SetTypeDefaultVersion' @ .
tsDefaultVersionId :: Lens' TypeSummary (Maybe Text)
tsDefaultVersionId = lens _tsDefaultVersionId (\s a -> s {_tsDefaultVersionId = a})

-- | The Amazon Resource Name (ARN) of the type.
tsTypeARN :: Lens' TypeSummary (Maybe Text)
tsTypeARN = lens _tsTypeARN (\s a -> s {_tsTypeARN = a})

-- | The kind of type.
tsType :: Lens' TypeSummary (Maybe RegistryType)
tsType = lens _tsType (\s a -> s {_tsType = a})

-- | The description of the type.
tsDescription :: Lens' TypeSummary (Maybe Text)
tsDescription = lens _tsDescription (\s a -> s {_tsDescription = a})

instance FromXML TypeSummary where
  parseXML x =
    TypeSummary'
      <$> (x .@? "LastUpdated")
      <*> (x .@? "TypeName")
      <*> (x .@? "DefaultVersionId")
      <*> (x .@? "TypeArn")
      <*> (x .@? "Type")
      <*> (x .@? "Description")

instance Hashable TypeSummary

instance NFData TypeSummary
