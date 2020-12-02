{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.Exclusion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.Exclusion where

import Network.AWS.Inspector.Types.Attribute
import Network.AWS.Inspector.Types.Scope
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about what was excluded from an assessment run.
--
--
--
-- /See:/ 'exclusion' smart constructor.
data Exclusion = Exclusion'
  { _eAttributes :: !(Maybe [Attribute]),
    _eArn :: !Text,
    _eTitle :: !Text,
    _eDescription :: !Text,
    _eRecommendation :: !Text,
    _eScopes :: !(List1 Scope)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Exclusion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eAttributes' - The system-defined attributes for the exclusion.
--
-- * 'eArn' - The ARN that specifies the exclusion.
--
-- * 'eTitle' - The name of the exclusion.
--
-- * 'eDescription' - The description of the exclusion.
--
-- * 'eRecommendation' - The recommendation for the exclusion.
--
-- * 'eScopes' - The AWS resources for which the exclusion pertains.
exclusion ::
  -- | 'eArn'
  Text ->
  -- | 'eTitle'
  Text ->
  -- | 'eDescription'
  Text ->
  -- | 'eRecommendation'
  Text ->
  -- | 'eScopes'
  NonEmpty Scope ->
  Exclusion
exclusion pArn_ pTitle_ pDescription_ pRecommendation_ pScopes_ =
  Exclusion'
    { _eAttributes = Nothing,
      _eArn = pArn_,
      _eTitle = pTitle_,
      _eDescription = pDescription_,
      _eRecommendation = pRecommendation_,
      _eScopes = _List1 # pScopes_
    }

-- | The system-defined attributes for the exclusion.
eAttributes :: Lens' Exclusion [Attribute]
eAttributes = lens _eAttributes (\s a -> s {_eAttributes = a}) . _Default . _Coerce

-- | The ARN that specifies the exclusion.
eArn :: Lens' Exclusion Text
eArn = lens _eArn (\s a -> s {_eArn = a})

-- | The name of the exclusion.
eTitle :: Lens' Exclusion Text
eTitle = lens _eTitle (\s a -> s {_eTitle = a})

-- | The description of the exclusion.
eDescription :: Lens' Exclusion Text
eDescription = lens _eDescription (\s a -> s {_eDescription = a})

-- | The recommendation for the exclusion.
eRecommendation :: Lens' Exclusion Text
eRecommendation = lens _eRecommendation (\s a -> s {_eRecommendation = a})

-- | The AWS resources for which the exclusion pertains.
eScopes :: Lens' Exclusion (NonEmpty Scope)
eScopes = lens _eScopes (\s a -> s {_eScopes = a}) . _List1

instance FromJSON Exclusion where
  parseJSON =
    withObject
      "Exclusion"
      ( \x ->
          Exclusion'
            <$> (x .:? "attributes" .!= mempty)
            <*> (x .: "arn")
            <*> (x .: "title")
            <*> (x .: "description")
            <*> (x .: "recommendation")
            <*> (x .: "scopes")
      )

instance Hashable Exclusion

instance NFData Exclusion
