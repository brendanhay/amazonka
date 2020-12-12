{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.InstanceIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.InstanceIdentity
  ( InstanceIdentity (..),

    -- * Smart constructor
    mkInstanceIdentity,

    -- * Lenses
    iiSignature,
    iiDocument,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains a description of an Amazon EC2 instance from the Amazon EC2 metadata service. For more information, see <https://docs.aws.amazon.com/sdkfornet/latest/apidocs/Index.html Instance Metadata and User Data> .
--
-- /See:/ 'mkInstanceIdentity' smart constructor.
data InstanceIdentity = InstanceIdentity'
  { signature ::
      Lude.Maybe Lude.Text,
    document :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceIdentity' with the minimum fields required to make a request.
--
-- * 'document' - A JSON document that contains the metadata.
-- * 'signature' - A signature that can be used to verify the document's accuracy and authenticity.
mkInstanceIdentity ::
  InstanceIdentity
mkInstanceIdentity =
  InstanceIdentity'
    { signature = Lude.Nothing,
      document = Lude.Nothing
    }

-- | A signature that can be used to verify the document's accuracy and authenticity.
--
-- /Note:/ Consider using 'signature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiSignature :: Lens.Lens' InstanceIdentity (Lude.Maybe Lude.Text)
iiSignature = Lens.lens (signature :: InstanceIdentity -> Lude.Maybe Lude.Text) (\s a -> s {signature = a} :: InstanceIdentity)
{-# DEPRECATED iiSignature "Use generic-lens or generic-optics with 'signature' instead." #-}

-- | A JSON document that contains the metadata.
--
-- /Note:/ Consider using 'document' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiDocument :: Lens.Lens' InstanceIdentity (Lude.Maybe Lude.Text)
iiDocument = Lens.lens (document :: InstanceIdentity -> Lude.Maybe Lude.Text) (\s a -> s {document = a} :: InstanceIdentity)
{-# DEPRECATED iiDocument "Use generic-lens or generic-optics with 'document' instead." #-}

instance Lude.ToJSON InstanceIdentity where
  toJSON InstanceIdentity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Signature" Lude..=) Lude.<$> signature,
            ("Document" Lude..=) Lude.<$> document
          ]
      )
