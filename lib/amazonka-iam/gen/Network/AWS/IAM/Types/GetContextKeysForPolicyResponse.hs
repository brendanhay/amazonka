{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.GetContextKeysForPolicyResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.GetContextKeysForPolicyResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the response to a successful 'GetContextKeysForPrincipalPolicy' or 'GetContextKeysForCustomPolicy' request.
--
--
--
-- /See:/ 'getContextKeysForPolicyResponse' smart constructor.
newtype GetContextKeysForPolicyResponse = GetContextKeysForPolicyResponse'
  { _gckfpContextKeyNames ::
      Maybe [Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetContextKeysForPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gckfpContextKeyNames' - The list of context keys that are referenced in the input policies.
getContextKeysForPolicyResponse ::
  GetContextKeysForPolicyResponse
getContextKeysForPolicyResponse =
  GetContextKeysForPolicyResponse' {_gckfpContextKeyNames = Nothing}

-- | The list of context keys that are referenced in the input policies.
gckfpContextKeyNames :: Lens' GetContextKeysForPolicyResponse [Text]
gckfpContextKeyNames = lens _gckfpContextKeyNames (\s a -> s {_gckfpContextKeyNames = a}) . _Default . _Coerce

instance FromXML GetContextKeysForPolicyResponse where
  parseXML x =
    GetContextKeysForPolicyResponse'
      <$> ( x .@? "ContextKeyNames" .!@ mempty
              >>= may (parseXMLList "member")
          )

instance Hashable GetContextKeysForPolicyResponse

instance NFData GetContextKeysForPolicyResponse
