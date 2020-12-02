{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.Type
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.Type where

import Network.AWS.AppSync.Types.TypeDefinitionFormat
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a type.
--
--
--
-- /See:/ 'type'' smart constructor.
data Type = Type'
  { _tArn :: !(Maybe Text),
    _tDefinition :: !(Maybe Text),
    _tFormat :: !(Maybe TypeDefinitionFormat),
    _tName :: !(Maybe Text),
    _tDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Type' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tArn' - The type ARN.
--
-- * 'tDefinition' - The type definition.
--
-- * 'tFormat' - The type format: SDL or JSON.
--
-- * 'tName' - The type name.
--
-- * 'tDescription' - The type description.
type' ::
  Type
type' =
  Type'
    { _tArn = Nothing,
      _tDefinition = Nothing,
      _tFormat = Nothing,
      _tName = Nothing,
      _tDescription = Nothing
    }

-- | The type ARN.
tArn :: Lens' Type (Maybe Text)
tArn = lens _tArn (\s a -> s {_tArn = a})

-- | The type definition.
tDefinition :: Lens' Type (Maybe Text)
tDefinition = lens _tDefinition (\s a -> s {_tDefinition = a})

-- | The type format: SDL or JSON.
tFormat :: Lens' Type (Maybe TypeDefinitionFormat)
tFormat = lens _tFormat (\s a -> s {_tFormat = a})

-- | The type name.
tName :: Lens' Type (Maybe Text)
tName = lens _tName (\s a -> s {_tName = a})

-- | The type description.
tDescription :: Lens' Type (Maybe Text)
tDescription = lens _tDescription (\s a -> s {_tDescription = a})

instance FromJSON Type where
  parseJSON =
    withObject
      "Type"
      ( \x ->
          Type'
            <$> (x .:? "arn")
            <*> (x .:? "definition")
            <*> (x .:? "format")
            <*> (x .:? "name")
            <*> (x .:? "description")
      )

instance Hashable Type

instance NFData Type
