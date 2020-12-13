{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Condition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Condition
  ( Condition (..),

    -- * Smart constructor
    mkCondition,

    -- * Lenses
    cKeyPrefixEquals,
    cHTTPErrorCodeReturnedEquals,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | A container for describing a condition that must be met for the specified redirect to apply. For example, 1. If request is for pages in the @/docs@ folder, redirect to the @/documents@ folder. 2. If request results in HTTP error 4xx, redirect request to another host where you might process the error.
--
-- /See:/ 'mkCondition' smart constructor.
data Condition = Condition'
  { -- | The object key name prefix when the redirect is applied. For example, to redirect requests for @ExamplePage.html@ , the key prefix will be @ExamplePage.html@ . To redirect request for all pages with the prefix @docs/@ , the key prefix will be @/docs@ , which identifies all objects in the @docs/@ folder. Required when the parent element @Condition@ is specified and sibling @HttpErrorCodeReturnedEquals@ is not specified. If both conditions are specified, both must be true for the redirect to be applied.
    keyPrefixEquals :: Lude.Maybe Lude.Text,
    -- | The HTTP error code when the redirect is applied. In the event of an error, if the error code equals this value, then the specified redirect is applied. Required when parent element @Condition@ is specified and sibling @KeyPrefixEquals@ is not specified. If both are specified, then both must be true for the redirect to be applied.
    hTTPErrorCodeReturnedEquals :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Condition' with the minimum fields required to make a request.
--
-- * 'keyPrefixEquals' - The object key name prefix when the redirect is applied. For example, to redirect requests for @ExamplePage.html@ , the key prefix will be @ExamplePage.html@ . To redirect request for all pages with the prefix @docs/@ , the key prefix will be @/docs@ , which identifies all objects in the @docs/@ folder. Required when the parent element @Condition@ is specified and sibling @HttpErrorCodeReturnedEquals@ is not specified. If both conditions are specified, both must be true for the redirect to be applied.
-- * 'hTTPErrorCodeReturnedEquals' - The HTTP error code when the redirect is applied. In the event of an error, if the error code equals this value, then the specified redirect is applied. Required when parent element @Condition@ is specified and sibling @KeyPrefixEquals@ is not specified. If both are specified, then both must be true for the redirect to be applied.
mkCondition ::
  Condition
mkCondition =
  Condition'
    { keyPrefixEquals = Lude.Nothing,
      hTTPErrorCodeReturnedEquals = Lude.Nothing
    }

-- | The object key name prefix when the redirect is applied. For example, to redirect requests for @ExamplePage.html@ , the key prefix will be @ExamplePage.html@ . To redirect request for all pages with the prefix @docs/@ , the key prefix will be @/docs@ , which identifies all objects in the @docs/@ folder. Required when the parent element @Condition@ is specified and sibling @HttpErrorCodeReturnedEquals@ is not specified. If both conditions are specified, both must be true for the redirect to be applied.
--
-- /Note:/ Consider using 'keyPrefixEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cKeyPrefixEquals :: Lens.Lens' Condition (Lude.Maybe Lude.Text)
cKeyPrefixEquals = Lens.lens (keyPrefixEquals :: Condition -> Lude.Maybe Lude.Text) (\s a -> s {keyPrefixEquals = a} :: Condition)
{-# DEPRECATED cKeyPrefixEquals "Use generic-lens or generic-optics with 'keyPrefixEquals' instead." #-}

-- | The HTTP error code when the redirect is applied. In the event of an error, if the error code equals this value, then the specified redirect is applied. Required when parent element @Condition@ is specified and sibling @KeyPrefixEquals@ is not specified. If both are specified, then both must be true for the redirect to be applied.
--
-- /Note:/ Consider using 'hTTPErrorCodeReturnedEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cHTTPErrorCodeReturnedEquals :: Lens.Lens' Condition (Lude.Maybe Lude.Text)
cHTTPErrorCodeReturnedEquals = Lens.lens (hTTPErrorCodeReturnedEquals :: Condition -> Lude.Maybe Lude.Text) (\s a -> s {hTTPErrorCodeReturnedEquals = a} :: Condition)
{-# DEPRECATED cHTTPErrorCodeReturnedEquals "Use generic-lens or generic-optics with 'hTTPErrorCodeReturnedEquals' instead." #-}

instance Lude.FromXML Condition where
  parseXML x =
    Condition'
      Lude.<$> (x Lude..@? "KeyPrefixEquals")
      Lude.<*> (x Lude..@? "HttpErrorCodeReturnedEquals")

instance Lude.ToXML Condition where
  toXML Condition' {..} =
    Lude.mconcat
      [ "KeyPrefixEquals" Lude.@= keyPrefixEquals,
        "HttpErrorCodeReturnedEquals" Lude.@= hTTPErrorCodeReturnedEquals
      ]
