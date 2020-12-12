{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BatchStatementRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BatchStatementRequest
  ( BatchStatementRequest (..),

    -- * Smart constructor
    mkBatchStatementRequest,

    -- * Lenses
    bsrConsistentRead,
    bsrParameters,
    bsrStatement,
  )
where

import Network.AWS.DynamoDB.Types.AttributeValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A PartiQL batch statement request.
--
-- /See:/ 'mkBatchStatementRequest' smart constructor.
data BatchStatementRequest = BatchStatementRequest'
  { consistentRead ::
      Lude.Maybe Lude.Bool,
    parameters ::
      Lude.Maybe (Lude.NonEmpty AttributeValue),
    statement :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchStatementRequest' with the minimum fields required to make a request.
--
-- * 'consistentRead' - The read consistency of the PartiQL batch request.
-- * 'parameters' - The parameters associated with a PartiQL statement in the batch request.
-- * 'statement' - A valid PartiQL statement.
mkBatchStatementRequest ::
  -- | 'statement'
  Lude.Text ->
  BatchStatementRequest
mkBatchStatementRequest pStatement_ =
  BatchStatementRequest'
    { consistentRead = Lude.Nothing,
      parameters = Lude.Nothing,
      statement = pStatement_
    }

-- | The read consistency of the PartiQL batch request.
--
-- /Note:/ Consider using 'consistentRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsrConsistentRead :: Lens.Lens' BatchStatementRequest (Lude.Maybe Lude.Bool)
bsrConsistentRead = Lens.lens (consistentRead :: BatchStatementRequest -> Lude.Maybe Lude.Bool) (\s a -> s {consistentRead = a} :: BatchStatementRequest)
{-# DEPRECATED bsrConsistentRead "Use generic-lens or generic-optics with 'consistentRead' instead." #-}

-- | The parameters associated with a PartiQL statement in the batch request.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsrParameters :: Lens.Lens' BatchStatementRequest (Lude.Maybe (Lude.NonEmpty AttributeValue))
bsrParameters = Lens.lens (parameters :: BatchStatementRequest -> Lude.Maybe (Lude.NonEmpty AttributeValue)) (\s a -> s {parameters = a} :: BatchStatementRequest)
{-# DEPRECATED bsrParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | A valid PartiQL statement.
--
-- /Note:/ Consider using 'statement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsrStatement :: Lens.Lens' BatchStatementRequest Lude.Text
bsrStatement = Lens.lens (statement :: BatchStatementRequest -> Lude.Text) (\s a -> s {statement = a} :: BatchStatementRequest)
{-# DEPRECATED bsrStatement "Use generic-lens or generic-optics with 'statement' instead." #-}

instance Lude.ToJSON BatchStatementRequest where
  toJSON BatchStatementRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ConsistentRead" Lude..=) Lude.<$> consistentRead,
            ("Parameters" Lude..=) Lude.<$> parameters,
            Lude.Just ("Statement" Lude..= statement)
          ]
      )
