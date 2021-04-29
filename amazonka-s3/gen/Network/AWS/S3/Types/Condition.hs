{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Condition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Condition where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

-- | A container for describing a condition that must be met for the
-- specified redirect to apply. For example, 1. If request is for pages in
-- the @\/docs@ folder, redirect to the @\/documents@ folder. 2. If request
-- results in HTTP error 4xx, redirect request to another host where you
-- might process the error.
--
-- /See:/ 'newCondition' smart constructor.
data Condition = Condition'
  { -- | The HTTP error code when the redirect is applied. In the event of an
    -- error, if the error code equals this value, then the specified redirect
    -- is applied. Required when parent element @Condition@ is specified and
    -- sibling @KeyPrefixEquals@ is not specified. If both are specified, then
    -- both must be true for the redirect to be applied.
    httpErrorCodeReturnedEquals :: Prelude.Maybe Prelude.Text,
    -- | The object key name prefix when the redirect is applied. For example, to
    -- redirect requests for @ExamplePage.html@, the key prefix will be
    -- @ExamplePage.html@. To redirect request for all pages with the prefix
    -- @docs\/@, the key prefix will be @\/docs@, which identifies all objects
    -- in the @docs\/@ folder. Required when the parent element @Condition@ is
    -- specified and sibling @HttpErrorCodeReturnedEquals@ is not specified. If
    -- both conditions are specified, both must be true for the redirect to be
    -- applied.
    --
    -- Replacement must be made for object keys containing special characters
    -- (such as carriage returns) when using XML requests. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
    keyPrefixEquals :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Condition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpErrorCodeReturnedEquals', 'condition_httpErrorCodeReturnedEquals' - The HTTP error code when the redirect is applied. In the event of an
-- error, if the error code equals this value, then the specified redirect
-- is applied. Required when parent element @Condition@ is specified and
-- sibling @KeyPrefixEquals@ is not specified. If both are specified, then
-- both must be true for the redirect to be applied.
--
-- 'keyPrefixEquals', 'condition_keyPrefixEquals' - The object key name prefix when the redirect is applied. For example, to
-- redirect requests for @ExamplePage.html@, the key prefix will be
-- @ExamplePage.html@. To redirect request for all pages with the prefix
-- @docs\/@, the key prefix will be @\/docs@, which identifies all objects
-- in the @docs\/@ folder. Required when the parent element @Condition@ is
-- specified and sibling @HttpErrorCodeReturnedEquals@ is not specified. If
-- both conditions are specified, both must be true for the redirect to be
-- applied.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
newCondition ::
  Condition
newCondition =
  Condition'
    { httpErrorCodeReturnedEquals =
        Prelude.Nothing,
      keyPrefixEquals = Prelude.Nothing
    }

-- | The HTTP error code when the redirect is applied. In the event of an
-- error, if the error code equals this value, then the specified redirect
-- is applied. Required when parent element @Condition@ is specified and
-- sibling @KeyPrefixEquals@ is not specified. If both are specified, then
-- both must be true for the redirect to be applied.
condition_httpErrorCodeReturnedEquals :: Lens.Lens' Condition (Prelude.Maybe Prelude.Text)
condition_httpErrorCodeReturnedEquals = Lens.lens (\Condition' {httpErrorCodeReturnedEquals} -> httpErrorCodeReturnedEquals) (\s@Condition' {} a -> s {httpErrorCodeReturnedEquals = a} :: Condition)

-- | The object key name prefix when the redirect is applied. For example, to
-- redirect requests for @ExamplePage.html@, the key prefix will be
-- @ExamplePage.html@. To redirect request for all pages with the prefix
-- @docs\/@, the key prefix will be @\/docs@, which identifies all objects
-- in the @docs\/@ folder. Required when the parent element @Condition@ is
-- specified and sibling @HttpErrorCodeReturnedEquals@ is not specified. If
-- both conditions are specified, both must be true for the redirect to be
-- applied.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
condition_keyPrefixEquals :: Lens.Lens' Condition (Prelude.Maybe Prelude.Text)
condition_keyPrefixEquals = Lens.lens (\Condition' {keyPrefixEquals} -> keyPrefixEquals) (\s@Condition' {} a -> s {keyPrefixEquals = a} :: Condition)

instance Prelude.FromXML Condition where
  parseXML x =
    Condition'
      Prelude.<$> (x Prelude..@? "HttpErrorCodeReturnedEquals")
      Prelude.<*> (x Prelude..@? "KeyPrefixEquals")

instance Prelude.Hashable Condition

instance Prelude.NFData Condition

instance Prelude.ToXML Condition where
  toXML Condition' {..} =
    Prelude.mconcat
      [ "HttpErrorCodeReturnedEquals"
          Prelude.@= httpErrorCodeReturnedEquals,
        "KeyPrefixEquals" Prelude.@= keyPrefixEquals
      ]
