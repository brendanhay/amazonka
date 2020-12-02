{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.ExclusionPreview
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.ExclusionPreview where

import Network.AWS.Inspector.Types.Attribute
import Network.AWS.Inspector.Types.Scope
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about what is excluded from an assessment run given the current state of the assessment template.
--
--
--
-- /See:/ 'exclusionPreview' smart constructor.
data ExclusionPreview = ExclusionPreview'
  { _epAttributes ::
      !(Maybe [Attribute]),
    _epTitle :: !Text,
    _epDescription :: !Text,
    _epRecommendation :: !Text,
    _epScopes :: !(List1 Scope)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExclusionPreview' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'epAttributes' - The system-defined attributes for the exclusion preview.
--
-- * 'epTitle' - The name of the exclusion preview.
--
-- * 'epDescription' - The description of the exclusion preview.
--
-- * 'epRecommendation' - The recommendation for the exclusion preview.
--
-- * 'epScopes' - The AWS resources for which the exclusion preview pertains.
exclusionPreview ::
  -- | 'epTitle'
  Text ->
  -- | 'epDescription'
  Text ->
  -- | 'epRecommendation'
  Text ->
  -- | 'epScopes'
  NonEmpty Scope ->
  ExclusionPreview
exclusionPreview pTitle_ pDescription_ pRecommendation_ pScopes_ =
  ExclusionPreview'
    { _epAttributes = Nothing,
      _epTitle = pTitle_,
      _epDescription = pDescription_,
      _epRecommendation = pRecommendation_,
      _epScopes = _List1 # pScopes_
    }

-- | The system-defined attributes for the exclusion preview.
epAttributes :: Lens' ExclusionPreview [Attribute]
epAttributes = lens _epAttributes (\s a -> s {_epAttributes = a}) . _Default . _Coerce

-- | The name of the exclusion preview.
epTitle :: Lens' ExclusionPreview Text
epTitle = lens _epTitle (\s a -> s {_epTitle = a})

-- | The description of the exclusion preview.
epDescription :: Lens' ExclusionPreview Text
epDescription = lens _epDescription (\s a -> s {_epDescription = a})

-- | The recommendation for the exclusion preview.
epRecommendation :: Lens' ExclusionPreview Text
epRecommendation = lens _epRecommendation (\s a -> s {_epRecommendation = a})

-- | The AWS resources for which the exclusion preview pertains.
epScopes :: Lens' ExclusionPreview (NonEmpty Scope)
epScopes = lens _epScopes (\s a -> s {_epScopes = a}) . _List1

instance FromJSON ExclusionPreview where
  parseJSON =
    withObject
      "ExclusionPreview"
      ( \x ->
          ExclusionPreview'
            <$> (x .:? "attributes" .!= mempty)
            <*> (x .: "title")
            <*> (x .: "description")
            <*> (x .: "recommendation")
            <*> (x .: "scopes")
      )

instance Hashable ExclusionPreview

instance NFData ExclusionPreview
