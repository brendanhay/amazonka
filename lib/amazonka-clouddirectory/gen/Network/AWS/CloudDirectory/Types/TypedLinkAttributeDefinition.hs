{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.TypedLinkAttributeDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.TypedLinkAttributeDefinition where

import Network.AWS.CloudDirectory.Types.FacetAttributeType
import Network.AWS.CloudDirectory.Types.RequiredAttributeBehavior
import Network.AWS.CloudDirectory.Types.Rule
import Network.AWS.CloudDirectory.Types.TypedAttributeValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A typed link attribute definition.
--
--
--
-- /See:/ 'typedLinkAttributeDefinition' smart constructor.
data TypedLinkAttributeDefinition = TypedLinkAttributeDefinition'
  { _tladRules ::
      !(Maybe (Map Text (Rule))),
    _tladDefaultValue ::
      !(Maybe TypedAttributeValue),
    _tladIsImmutable :: !(Maybe Bool),
    _tladName :: !Text,
    _tladType :: !FacetAttributeType,
    _tladRequiredBehavior ::
      !RequiredAttributeBehavior
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TypedLinkAttributeDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tladRules' - Validation rules that are attached to the attribute definition.
--
-- * 'tladDefaultValue' - The default value of the attribute (if configured).
--
-- * 'tladIsImmutable' - Whether the attribute is mutable or not.
--
-- * 'tladName' - The unique name of the typed link attribute.
--
-- * 'tladType' - The type of the attribute.
--
-- * 'tladRequiredBehavior' - The required behavior of the @TypedLinkAttributeDefinition@ .
typedLinkAttributeDefinition ::
  -- | 'tladName'
  Text ->
  -- | 'tladType'
  FacetAttributeType ->
  -- | 'tladRequiredBehavior'
  RequiredAttributeBehavior ->
  TypedLinkAttributeDefinition
typedLinkAttributeDefinition pName_ pType_ pRequiredBehavior_ =
  TypedLinkAttributeDefinition'
    { _tladRules = Nothing,
      _tladDefaultValue = Nothing,
      _tladIsImmutable = Nothing,
      _tladName = pName_,
      _tladType = pType_,
      _tladRequiredBehavior = pRequiredBehavior_
    }

-- | Validation rules that are attached to the attribute definition.
tladRules :: Lens' TypedLinkAttributeDefinition (HashMap Text (Rule))
tladRules = lens _tladRules (\s a -> s {_tladRules = a}) . _Default . _Map

-- | The default value of the attribute (if configured).
tladDefaultValue :: Lens' TypedLinkAttributeDefinition (Maybe TypedAttributeValue)
tladDefaultValue = lens _tladDefaultValue (\s a -> s {_tladDefaultValue = a})

-- | Whether the attribute is mutable or not.
tladIsImmutable :: Lens' TypedLinkAttributeDefinition (Maybe Bool)
tladIsImmutable = lens _tladIsImmutable (\s a -> s {_tladIsImmutable = a})

-- | The unique name of the typed link attribute.
tladName :: Lens' TypedLinkAttributeDefinition Text
tladName = lens _tladName (\s a -> s {_tladName = a})

-- | The type of the attribute.
tladType :: Lens' TypedLinkAttributeDefinition FacetAttributeType
tladType = lens _tladType (\s a -> s {_tladType = a})

-- | The required behavior of the @TypedLinkAttributeDefinition@ .
tladRequiredBehavior :: Lens' TypedLinkAttributeDefinition RequiredAttributeBehavior
tladRequiredBehavior = lens _tladRequiredBehavior (\s a -> s {_tladRequiredBehavior = a})

instance FromJSON TypedLinkAttributeDefinition where
  parseJSON =
    withObject
      "TypedLinkAttributeDefinition"
      ( \x ->
          TypedLinkAttributeDefinition'
            <$> (x .:? "Rules" .!= mempty)
            <*> (x .:? "DefaultValue")
            <*> (x .:? "IsImmutable")
            <*> (x .: "Name")
            <*> (x .: "Type")
            <*> (x .: "RequiredBehavior")
      )

instance Hashable TypedLinkAttributeDefinition

instance NFData TypedLinkAttributeDefinition

instance ToJSON TypedLinkAttributeDefinition where
  toJSON TypedLinkAttributeDefinition' {..} =
    object
      ( catMaybes
          [ ("Rules" .=) <$> _tladRules,
            ("DefaultValue" .=) <$> _tladDefaultValue,
            ("IsImmutable" .=) <$> _tladIsImmutable,
            Just ("Name" .= _tladName),
            Just ("Type" .= _tladType),
            Just ("RequiredBehavior" .= _tladRequiredBehavior)
          ]
      )
