-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.Source
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.Source
  ( Source (..),

    -- * Smart constructor
    mkSource,

    -- * Lenses
    sSourceDetails,
    sOwner,
    sSourceIdentifier,
  )
where

import Network.AWS.Config.Types.Owner
import Network.AWS.Config.Types.SourceDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the AWS Config rule owner (AWS or customer), the rule identifier, and the events that trigger the evaluation of your AWS resources.
--
-- /See:/ 'mkSource' smart constructor.
data Source = Source'
  { sourceDetails :: Lude.Maybe [SourceDetail],
    owner :: Owner,
    sourceIdentifier :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Source' with the minimum fields required to make a request.
--
-- * 'owner' - Indicates whether AWS or the customer owns and manages the AWS Config rule.
-- * 'sourceDetails' - Provides the source and type of the event that causes AWS Config to evaluate your AWS resources.
-- * 'sourceIdentifier' - For AWS Config managed rules, a predefined identifier from a list. For example, @IAM_PASSWORD_POLICY@ is a managed rule. To reference a managed rule, see <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html Using AWS Managed Config Rules> .
--
-- For custom rules, the identifier is the Amazon Resource Name (ARN) of the rule's AWS Lambda function, such as @arn:aws:lambda:us-east-2:123456789012:function:custom_rule_name@ .
mkSource ::
  -- | 'owner'
  Owner ->
  -- | 'sourceIdentifier'
  Lude.Text ->
  Source
mkSource pOwner_ pSourceIdentifier_ =
  Source'
    { sourceDetails = Lude.Nothing,
      owner = pOwner_,
      sourceIdentifier = pSourceIdentifier_
    }

-- | Provides the source and type of the event that causes AWS Config to evaluate your AWS resources.
--
-- /Note:/ Consider using 'sourceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSourceDetails :: Lens.Lens' Source (Lude.Maybe [SourceDetail])
sSourceDetails = Lens.lens (sourceDetails :: Source -> Lude.Maybe [SourceDetail]) (\s a -> s {sourceDetails = a} :: Source)
{-# DEPRECATED sSourceDetails "Use generic-lens or generic-optics with 'sourceDetails' instead." #-}

-- | Indicates whether AWS or the customer owns and manages the AWS Config rule.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOwner :: Lens.Lens' Source Owner
sOwner = Lens.lens (owner :: Source -> Owner) (\s a -> s {owner = a} :: Source)
{-# DEPRECATED sOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | For AWS Config managed rules, a predefined identifier from a list. For example, @IAM_PASSWORD_POLICY@ is a managed rule. To reference a managed rule, see <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html Using AWS Managed Config Rules> .
--
-- For custom rules, the identifier is the Amazon Resource Name (ARN) of the rule's AWS Lambda function, such as @arn:aws:lambda:us-east-2:123456789012:function:custom_rule_name@ .
--
-- /Note:/ Consider using 'sourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSourceIdentifier :: Lens.Lens' Source Lude.Text
sSourceIdentifier = Lens.lens (sourceIdentifier :: Source -> Lude.Text) (\s a -> s {sourceIdentifier = a} :: Source)
{-# DEPRECATED sSourceIdentifier "Use generic-lens or generic-optics with 'sourceIdentifier' instead." #-}

instance Lude.FromJSON Source where
  parseJSON =
    Lude.withObject
      "Source"
      ( \x ->
          Source'
            Lude.<$> (x Lude..:? "SourceDetails" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "Owner")
            Lude.<*> (x Lude..: "SourceIdentifier")
      )

instance Lude.ToJSON Source where
  toJSON Source' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SourceDetails" Lude..=) Lude.<$> sourceDetails,
            Lude.Just ("Owner" Lude..= owner),
            Lude.Just ("SourceIdentifier" Lude..= sourceIdentifier)
          ]
      )
