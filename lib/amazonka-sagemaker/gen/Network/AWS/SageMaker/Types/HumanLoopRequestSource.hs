{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HumanLoopRequestSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HumanLoopRequestSource
  ( HumanLoopRequestSource (..),

    -- * Smart constructor
    mkHumanLoopRequestSource,

    -- * Lenses
    hlrsAWSManagedHumanLoopRequestSource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.AWSManagedHumanLoopRequestSource

-- | Container for configuring the source of human task requests.
--
-- /See:/ 'mkHumanLoopRequestSource' smart constructor.
newtype HumanLoopRequestSource = HumanLoopRequestSource'
  { awsManagedHumanLoopRequestSource ::
      AWSManagedHumanLoopRequestSource
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HumanLoopRequestSource' with the minimum fields required to make a request.
--
-- * 'awsManagedHumanLoopRequestSource' - Specifies whether Amazon Rekognition or Amazon Textract are used as the integration source. The default field settings and JSON parsing rules are different based on the integration source. Valid values:
mkHumanLoopRequestSource ::
  -- | 'awsManagedHumanLoopRequestSource'
  AWSManagedHumanLoopRequestSource ->
  HumanLoopRequestSource
mkHumanLoopRequestSource pAWSManagedHumanLoopRequestSource_ =
  HumanLoopRequestSource'
    { awsManagedHumanLoopRequestSource =
        pAWSManagedHumanLoopRequestSource_
    }

-- | Specifies whether Amazon Rekognition or Amazon Textract are used as the integration source. The default field settings and JSON parsing rules are different based on the integration source. Valid values:
--
-- /Note:/ Consider using 'awsManagedHumanLoopRequestSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlrsAWSManagedHumanLoopRequestSource :: Lens.Lens' HumanLoopRequestSource AWSManagedHumanLoopRequestSource
hlrsAWSManagedHumanLoopRequestSource = Lens.lens (awsManagedHumanLoopRequestSource :: HumanLoopRequestSource -> AWSManagedHumanLoopRequestSource) (\s a -> s {awsManagedHumanLoopRequestSource = a} :: HumanLoopRequestSource)
{-# DEPRECATED hlrsAWSManagedHumanLoopRequestSource "Use generic-lens or generic-optics with 'awsManagedHumanLoopRequestSource' instead." #-}

instance Lude.FromJSON HumanLoopRequestSource where
  parseJSON =
    Lude.withObject
      "HumanLoopRequestSource"
      ( \x ->
          HumanLoopRequestSource'
            Lude.<$> (x Lude..: "AwsManagedHumanLoopRequestSource")
      )

instance Lude.ToJSON HumanLoopRequestSource where
  toJSON HumanLoopRequestSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "AwsManagedHumanLoopRequestSource"
                  Lude..= awsManagedHumanLoopRequestSource
              )
          ]
      )
