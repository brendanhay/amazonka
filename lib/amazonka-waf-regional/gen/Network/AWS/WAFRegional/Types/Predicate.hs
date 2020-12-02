{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.Predicate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.Predicate where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAFRegional.Types.PredicateType

-- | Specifies the 'ByteMatchSet' , 'IPSet' , 'SqlInjectionMatchSet' , 'XssMatchSet' , 'RegexMatchSet' , 'GeoMatchSet' , and 'SizeConstraintSet' objects that you want to add to a @Rule@ and, for each object, indicates whether you want to negate the settings, for example, requests that do NOT originate from the IP address 192.0.2.44.
--
--
--
-- /See:/ 'predicate' smart constructor.
data Predicate = Predicate'
  { _pNegated :: !Bool,
    _pType :: !PredicateType,
    _pDataId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Predicate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pNegated' - Set @Negated@ to @False@ if you want AWS WAF to allow, block, or count requests based on the settings in the specified 'ByteMatchSet' , 'IPSet' , 'SqlInjectionMatchSet' , 'XssMatchSet' , 'RegexMatchSet' , 'GeoMatchSet' , or 'SizeConstraintSet' . For example, if an @IPSet@ includes the IP address @192.0.2.44@ , AWS WAF will allow or block requests based on that IP address. Set @Negated@ to @True@ if you want AWS WAF to allow or block a request based on the negation of the settings in the 'ByteMatchSet' , 'IPSet' , 'SqlInjectionMatchSet' , 'XssMatchSet' , 'RegexMatchSet' , 'GeoMatchSet' , or 'SizeConstraintSet' . For example, if an @IPSet@ includes the IP address @192.0.2.44@ , AWS WAF will allow, block, or count requests based on all IP addresses /except/ @192.0.2.44@ .
--
-- * 'pType' - The type of predicate in a @Rule@ , such as @ByteMatch@ or @IPSet@ .
--
-- * 'pDataId' - A unique identifier for a predicate in a @Rule@ , such as @ByteMatchSetId@ or @IPSetId@ . The ID is returned by the corresponding @Create@ or @List@ command.
predicate ::
  -- | 'pNegated'
  Bool ->
  -- | 'pType'
  PredicateType ->
  -- | 'pDataId'
  Text ->
  Predicate
predicate pNegated_ pType_ pDataId_ =
  Predicate'
    { _pNegated = pNegated_,
      _pType = pType_,
      _pDataId = pDataId_
    }

-- | Set @Negated@ to @False@ if you want AWS WAF to allow, block, or count requests based on the settings in the specified 'ByteMatchSet' , 'IPSet' , 'SqlInjectionMatchSet' , 'XssMatchSet' , 'RegexMatchSet' , 'GeoMatchSet' , or 'SizeConstraintSet' . For example, if an @IPSet@ includes the IP address @192.0.2.44@ , AWS WAF will allow or block requests based on that IP address. Set @Negated@ to @True@ if you want AWS WAF to allow or block a request based on the negation of the settings in the 'ByteMatchSet' , 'IPSet' , 'SqlInjectionMatchSet' , 'XssMatchSet' , 'RegexMatchSet' , 'GeoMatchSet' , or 'SizeConstraintSet' . For example, if an @IPSet@ includes the IP address @192.0.2.44@ , AWS WAF will allow, block, or count requests based on all IP addresses /except/ @192.0.2.44@ .
pNegated :: Lens' Predicate Bool
pNegated = lens _pNegated (\s a -> s {_pNegated = a})

-- | The type of predicate in a @Rule@ , such as @ByteMatch@ or @IPSet@ .
pType :: Lens' Predicate PredicateType
pType = lens _pType (\s a -> s {_pType = a})

-- | A unique identifier for a predicate in a @Rule@ , such as @ByteMatchSetId@ or @IPSetId@ . The ID is returned by the corresponding @Create@ or @List@ command.
pDataId :: Lens' Predicate Text
pDataId = lens _pDataId (\s a -> s {_pDataId = a})

instance FromJSON Predicate where
  parseJSON =
    withObject
      "Predicate"
      ( \x ->
          Predicate'
            <$> (x .: "Negated") <*> (x .: "Type") <*> (x .: "DataId")
      )

instance Hashable Predicate

instance NFData Predicate

instance ToJSON Predicate where
  toJSON Predicate' {..} =
    object
      ( catMaybes
          [ Just ("Negated" .= _pNegated),
            Just ("Type" .= _pType),
            Just ("DataId" .= _pDataId)
          ]
      )
