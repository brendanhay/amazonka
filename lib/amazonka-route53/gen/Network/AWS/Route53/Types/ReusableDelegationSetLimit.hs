{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.ReusableDelegationSetLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.ReusableDelegationSetLimit where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.ReusableDelegationSetLimitType

-- | A complex type that contains the type of limit that you specified in the request and the current value for that limit.
--
--
--
-- /See:/ 'reusableDelegationSetLimit' smart constructor.
data ReusableDelegationSetLimit = ReusableDelegationSetLimit'
  { _rdslType ::
      !ReusableDelegationSetLimitType,
    _rdslValue :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReusableDelegationSetLimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdslType' - The limit that you requested: @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ , the maximum number of hosted zones that you can associate with the specified reusable delegation set.
--
-- * 'rdslValue' - The current value for the @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ limit.
reusableDelegationSetLimit ::
  -- | 'rdslType'
  ReusableDelegationSetLimitType ->
  -- | 'rdslValue'
  Natural ->
  ReusableDelegationSetLimit
reusableDelegationSetLimit pType_ pValue_ =
  ReusableDelegationSetLimit'
    { _rdslType = pType_,
      _rdslValue = _Nat # pValue_
    }

-- | The limit that you requested: @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ , the maximum number of hosted zones that you can associate with the specified reusable delegation set.
rdslType :: Lens' ReusableDelegationSetLimit ReusableDelegationSetLimitType
rdslType = lens _rdslType (\s a -> s {_rdslType = a})

-- | The current value for the @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ limit.
rdslValue :: Lens' ReusableDelegationSetLimit Natural
rdslValue = lens _rdslValue (\s a -> s {_rdslValue = a}) . _Nat

instance FromXML ReusableDelegationSetLimit where
  parseXML x =
    ReusableDelegationSetLimit' <$> (x .@ "Type") <*> (x .@ "Value")

instance Hashable ReusableDelegationSetLimit

instance NFData ReusableDelegationSetLimit
