{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.IdentityPoolShortDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.IdentityPoolShortDescription
  ( IdentityPoolShortDescription (..),

    -- * Smart constructor
    mkIdentityPoolShortDescription,

    -- * Lenses
    ipsdIdentityPoolId,
    ipsdIdentityPoolName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A description of the identity pool.
--
-- /See:/ 'mkIdentityPoolShortDescription' smart constructor.
data IdentityPoolShortDescription = IdentityPoolShortDescription'
  { identityPoolId ::
      Lude.Maybe Lude.Text,
    identityPoolName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IdentityPoolShortDescription' with the minimum fields required to make a request.
--
-- * 'identityPoolId' - An identity pool ID in the format REGION:GUID.
-- * 'identityPoolName' - A string that you provide.
mkIdentityPoolShortDescription ::
  IdentityPoolShortDescription
mkIdentityPoolShortDescription =
  IdentityPoolShortDescription'
    { identityPoolId = Lude.Nothing,
      identityPoolName = Lude.Nothing
    }

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsdIdentityPoolId :: Lens.Lens' IdentityPoolShortDescription (Lude.Maybe Lude.Text)
ipsdIdentityPoolId = Lens.lens (identityPoolId :: IdentityPoolShortDescription -> Lude.Maybe Lude.Text) (\s a -> s {identityPoolId = a} :: IdentityPoolShortDescription)
{-# DEPRECATED ipsdIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | A string that you provide.
--
-- /Note:/ Consider using 'identityPoolName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsdIdentityPoolName :: Lens.Lens' IdentityPoolShortDescription (Lude.Maybe Lude.Text)
ipsdIdentityPoolName = Lens.lens (identityPoolName :: IdentityPoolShortDescription -> Lude.Maybe Lude.Text) (\s a -> s {identityPoolName = a} :: IdentityPoolShortDescription)
{-# DEPRECATED ipsdIdentityPoolName "Use generic-lens or generic-optics with 'identityPoolName' instead." #-}

instance Lude.FromJSON IdentityPoolShortDescription where
  parseJSON =
    Lude.withObject
      "IdentityPoolShortDescription"
      ( \x ->
          IdentityPoolShortDescription'
            Lude.<$> (x Lude..:? "IdentityPoolId")
            Lude.<*> (x Lude..:? "IdentityPoolName")
      )
