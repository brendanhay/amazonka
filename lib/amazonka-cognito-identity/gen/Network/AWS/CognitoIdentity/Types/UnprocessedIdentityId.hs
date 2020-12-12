{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.UnprocessedIdentityId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.UnprocessedIdentityId
  ( UnprocessedIdentityId (..),

    -- * Smart constructor
    mkUnprocessedIdentityId,

    -- * Lenses
    uiiErrorCode,
    uiiIdentityId,
  )
where

import Network.AWS.CognitoIdentity.Types.CognitoErrorCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An array of UnprocessedIdentityId objects, each of which contains an ErrorCode and IdentityId.
--
-- /See:/ 'mkUnprocessedIdentityId' smart constructor.
data UnprocessedIdentityId = UnprocessedIdentityId'
  { errorCode ::
      Lude.Maybe CognitoErrorCode,
    identityId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnprocessedIdentityId' with the minimum fields required to make a request.
--
-- * 'errorCode' - The error code indicating the type of error that occurred.
-- * 'identityId' - A unique identifier in the format REGION:GUID.
mkUnprocessedIdentityId ::
  UnprocessedIdentityId
mkUnprocessedIdentityId =
  UnprocessedIdentityId'
    { errorCode = Lude.Nothing,
      identityId = Lude.Nothing
    }

-- | The error code indicating the type of error that occurred.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiiErrorCode :: Lens.Lens' UnprocessedIdentityId (Lude.Maybe CognitoErrorCode)
uiiErrorCode = Lens.lens (errorCode :: UnprocessedIdentityId -> Lude.Maybe CognitoErrorCode) (\s a -> s {errorCode = a} :: UnprocessedIdentityId)
{-# DEPRECATED uiiErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | A unique identifier in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiiIdentityId :: Lens.Lens' UnprocessedIdentityId (Lude.Maybe Lude.Text)
uiiIdentityId = Lens.lens (identityId :: UnprocessedIdentityId -> Lude.Maybe Lude.Text) (\s a -> s {identityId = a} :: UnprocessedIdentityId)
{-# DEPRECATED uiiIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

instance Lude.FromJSON UnprocessedIdentityId where
  parseJSON =
    Lude.withObject
      "UnprocessedIdentityId"
      ( \x ->
          UnprocessedIdentityId'
            Lude.<$> (x Lude..:? "ErrorCode") Lude.<*> (x Lude..:? "IdentityId")
      )
