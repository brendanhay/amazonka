{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.InstanceIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.InstanceIdentity
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

-- | Identity information for the EC2 instance that is hosting the task runner. You can get this value by calling a metadata URI from the EC2 instance. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AESDG-chapter-instancedata.html Instance Metadata> in the /Amazon Elastic Compute Cloud User Guide./ Passing in this value proves that your task runner is running on an EC2 instance, and ensures the proper AWS Data Pipeline service charges are applied to your pipeline.
--
--
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
-- * 'document' - A description of an EC2 instance that is generated when the instance is launched and exposed to the instance via the instance metadata service in the form of a JSON representation of an object.
-- * 'signature' - A signature which can be used to verify the accuracy and authenticity of the information provided in the instance identity document.
mkInstanceIdentity ::
  InstanceIdentity
mkInstanceIdentity =
  InstanceIdentity'
    { signature = Lude.Nothing,
      document = Lude.Nothing
    }

-- | A signature which can be used to verify the accuracy and authenticity of the information provided in the instance identity document.
--
-- /Note:/ Consider using 'signature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiSignature :: Lens.Lens' InstanceIdentity (Lude.Maybe Lude.Text)
iiSignature = Lens.lens (signature :: InstanceIdentity -> Lude.Maybe Lude.Text) (\s a -> s {signature = a} :: InstanceIdentity)
{-# DEPRECATED iiSignature "Use generic-lens or generic-optics with 'signature' instead." #-}

-- | A description of an EC2 instance that is generated when the instance is launched and exposed to the instance via the instance metadata service in the form of a JSON representation of an object.
--
-- /Note:/ Consider using 'document' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiDocument :: Lens.Lens' InstanceIdentity (Lude.Maybe Lude.Text)
iiDocument = Lens.lens (document :: InstanceIdentity -> Lude.Maybe Lude.Text) (\s a -> s {document = a} :: InstanceIdentity)
{-# DEPRECATED iiDocument "Use generic-lens or generic-optics with 'document' instead." #-}

instance Lude.ToJSON InstanceIdentity where
  toJSON InstanceIdentity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("signature" Lude..=) Lude.<$> signature,
            ("document" Lude..=) Lude.<$> document
          ]
      )
