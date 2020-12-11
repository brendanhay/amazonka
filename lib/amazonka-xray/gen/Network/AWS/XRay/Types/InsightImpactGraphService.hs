-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.InsightImpactGraphService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.InsightImpactGraphService
  ( InsightImpactGraphService (..),

    -- * Smart constructor
    mkInsightImpactGraphService,

    -- * Lenses
    iigsReferenceId,
    iigsAccountId,
    iigsNames,
    iigsName,
    iigsType,
    iigsEdges,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.XRay.Types.InsightImpactGraphEdge

-- | Information about an application that processed requests, users that made requests, or downstream services, resources, and applications that an application used.
--
-- /See:/ 'mkInsightImpactGraphService' smart constructor.
data InsightImpactGraphService = InsightImpactGraphService'
  { referenceId ::
      Lude.Maybe Lude.Int,
    accountId :: Lude.Maybe Lude.Text,
    names :: Lude.Maybe [Lude.Text],
    name :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe Lude.Text,
    edges ::
      Lude.Maybe [InsightImpactGraphEdge]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InsightImpactGraphService' with the minimum fields required to make a request.
--
-- * 'accountId' - Identifier of the AWS account in which the service runs.
-- * 'edges' - Connections to downstream services.
-- * 'name' - The canonical name of the service.
-- * 'names' - A list of names for the service, including the canonical name.
-- * 'referenceId' - Identifier for the service. Unique within the service map.
-- * 'type'' - Identifier for the service. Unique within the service map.
--
--
--     * AWS Resource - The type of an AWS resource. For example, AWS::EC2::Instance for an application running on Amazon EC2 or AWS::DynamoDB::Table for an Amazon DynamoDB table that the application used.
--
--
--     * AWS Service - The type of an AWS service. For example, AWS::DynamoDB for downstream calls to Amazon DynamoDB that didn't target a specific table.
--
--
--     * AWS Service - The type of an AWS service. For example, AWS::DynamoDB for downstream calls to Amazon DynamoDB that didn't target a specific table.
--
--
--     * remote - A downstream service of indeterminate type.
mkInsightImpactGraphService ::
  InsightImpactGraphService
mkInsightImpactGraphService =
  InsightImpactGraphService'
    { referenceId = Lude.Nothing,
      accountId = Lude.Nothing,
      names = Lude.Nothing,
      name = Lude.Nothing,
      type' = Lude.Nothing,
      edges = Lude.Nothing
    }

-- | Identifier for the service. Unique within the service map.
--
-- /Note:/ Consider using 'referenceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iigsReferenceId :: Lens.Lens' InsightImpactGraphService (Lude.Maybe Lude.Int)
iigsReferenceId = Lens.lens (referenceId :: InsightImpactGraphService -> Lude.Maybe Lude.Int) (\s a -> s {referenceId = a} :: InsightImpactGraphService)
{-# DEPRECATED iigsReferenceId "Use generic-lens or generic-optics with 'referenceId' instead." #-}

-- | Identifier of the AWS account in which the service runs.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iigsAccountId :: Lens.Lens' InsightImpactGraphService (Lude.Maybe Lude.Text)
iigsAccountId = Lens.lens (accountId :: InsightImpactGraphService -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: InsightImpactGraphService)
{-# DEPRECATED iigsAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | A list of names for the service, including the canonical name.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iigsNames :: Lens.Lens' InsightImpactGraphService (Lude.Maybe [Lude.Text])
iigsNames = Lens.lens (names :: InsightImpactGraphService -> Lude.Maybe [Lude.Text]) (\s a -> s {names = a} :: InsightImpactGraphService)
{-# DEPRECATED iigsNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | The canonical name of the service.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iigsName :: Lens.Lens' InsightImpactGraphService (Lude.Maybe Lude.Text)
iigsName = Lens.lens (name :: InsightImpactGraphService -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: InsightImpactGraphService)
{-# DEPRECATED iigsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Identifier for the service. Unique within the service map.
--
--
--     * AWS Resource - The type of an AWS resource. For example, AWS::EC2::Instance for an application running on Amazon EC2 or AWS::DynamoDB::Table for an Amazon DynamoDB table that the application used.
--
--
--     * AWS Service - The type of an AWS service. For example, AWS::DynamoDB for downstream calls to Amazon DynamoDB that didn't target a specific table.
--
--
--     * AWS Service - The type of an AWS service. For example, AWS::DynamoDB for downstream calls to Amazon DynamoDB that didn't target a specific table.
--
--
--     * remote - A downstream service of indeterminate type.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iigsType :: Lens.Lens' InsightImpactGraphService (Lude.Maybe Lude.Text)
iigsType = Lens.lens (type' :: InsightImpactGraphService -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: InsightImpactGraphService)
{-# DEPRECATED iigsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Connections to downstream services.
--
-- /Note:/ Consider using 'edges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iigsEdges :: Lens.Lens' InsightImpactGraphService (Lude.Maybe [InsightImpactGraphEdge])
iigsEdges = Lens.lens (edges :: InsightImpactGraphService -> Lude.Maybe [InsightImpactGraphEdge]) (\s a -> s {edges = a} :: InsightImpactGraphService)
{-# DEPRECATED iigsEdges "Use generic-lens or generic-optics with 'edges' instead." #-}

instance Lude.FromJSON InsightImpactGraphService where
  parseJSON =
    Lude.withObject
      "InsightImpactGraphService"
      ( \x ->
          InsightImpactGraphService'
            Lude.<$> (x Lude..:? "ReferenceId")
            Lude.<*> (x Lude..:? "AccountId")
            Lude.<*> (x Lude..:? "Names" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "Edges" Lude..!= Lude.mempty)
      )
