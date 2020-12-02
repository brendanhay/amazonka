{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchDetachTypedLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchDetachTypedLink where

import Network.AWS.CloudDirectory.Types.TypedLinkSpecifier
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Detaches a typed link from a specified source and target object inside a 'BatchRead' operation. For more information, see 'DetachTypedLink' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchDetachTypedLink' smart constructor.
newtype BatchDetachTypedLink = BatchDetachTypedLink'
  { _bdtlTypedLinkSpecifier ::
      TypedLinkSpecifier
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchDetachTypedLink' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdtlTypedLinkSpecifier' - Used to accept a typed link specifier as input.
batchDetachTypedLink ::
  -- | 'bdtlTypedLinkSpecifier'
  TypedLinkSpecifier ->
  BatchDetachTypedLink
batchDetachTypedLink pTypedLinkSpecifier_ =
  BatchDetachTypedLink'
    { _bdtlTypedLinkSpecifier =
        pTypedLinkSpecifier_
    }

-- | Used to accept a typed link specifier as input.
bdtlTypedLinkSpecifier :: Lens' BatchDetachTypedLink TypedLinkSpecifier
bdtlTypedLinkSpecifier = lens _bdtlTypedLinkSpecifier (\s a -> s {_bdtlTypedLinkSpecifier = a})

instance Hashable BatchDetachTypedLink

instance NFData BatchDetachTypedLink

instance ToJSON BatchDetachTypedLink where
  toJSON BatchDetachTypedLink' {..} =
    object
      ( catMaybes
          [Just ("TypedLinkSpecifier" .= _bdtlTypedLinkSpecifier)]
      )
