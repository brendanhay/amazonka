{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.MappingRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.MappingRule where

import Network.AWS.CognitoIdentity.Types.MappingRuleMatchType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A rule that maps a claim name, a claim value, and a match type to a role ARN.
--
--
--
-- /See:/ 'mappingRule' smart constructor.
data MappingRule = MappingRule'
  { _mrClaim :: !Text,
    _mrMatchType :: !MappingRuleMatchType,
    _mrValue :: !Text,
    _mrRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MappingRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrClaim' - The claim name that must be present in the token, for example, "isAdmin" or "paid".
--
-- * 'mrMatchType' - The match condition that specifies how closely the claim value in the IdP token must match @Value@ .
--
-- * 'mrValue' - A brief string that the claim must match, for example, "paid" or "yes".
--
-- * 'mrRoleARN' - The role ARN.
mappingRule ::
  -- | 'mrClaim'
  Text ->
  -- | 'mrMatchType'
  MappingRuleMatchType ->
  -- | 'mrValue'
  Text ->
  -- | 'mrRoleARN'
  Text ->
  MappingRule
mappingRule pClaim_ pMatchType_ pValue_ pRoleARN_ =
  MappingRule'
    { _mrClaim = pClaim_,
      _mrMatchType = pMatchType_,
      _mrValue = pValue_,
      _mrRoleARN = pRoleARN_
    }

-- | The claim name that must be present in the token, for example, "isAdmin" or "paid".
mrClaim :: Lens' MappingRule Text
mrClaim = lens _mrClaim (\s a -> s {_mrClaim = a})

-- | The match condition that specifies how closely the claim value in the IdP token must match @Value@ .
mrMatchType :: Lens' MappingRule MappingRuleMatchType
mrMatchType = lens _mrMatchType (\s a -> s {_mrMatchType = a})

-- | A brief string that the claim must match, for example, "paid" or "yes".
mrValue :: Lens' MappingRule Text
mrValue = lens _mrValue (\s a -> s {_mrValue = a})

-- | The role ARN.
mrRoleARN :: Lens' MappingRule Text
mrRoleARN = lens _mrRoleARN (\s a -> s {_mrRoleARN = a})

instance FromJSON MappingRule where
  parseJSON =
    withObject
      "MappingRule"
      ( \x ->
          MappingRule'
            <$> (x .: "Claim")
            <*> (x .: "MatchType")
            <*> (x .: "Value")
            <*> (x .: "RoleARN")
      )

instance Hashable MappingRule

instance NFData MappingRule

instance ToJSON MappingRule where
  toJSON MappingRule' {..} =
    object
      ( catMaybes
          [ Just ("Claim" .= _mrClaim),
            Just ("MatchType" .= _mrMatchType),
            Just ("Value" .= _mrValue),
            Just ("RoleARN" .= _mrRoleARN)
          ]
      )
