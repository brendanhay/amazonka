-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginGroupFailoverCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginGroupFailoverCriteria
  ( OriginGroupFailoverCriteria (..),

    -- * Smart constructor
    mkOriginGroupFailoverCriteria,

    -- * Lenses
    ogfcStatusCodes,
  )
where

import Network.AWS.CloudFront.Types.StatusCodes
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex data type that includes information about the failover criteria for an origin group, including the status codes for which CloudFront will failover from the primary origin to the second origin.
--
-- /See:/ 'mkOriginGroupFailoverCriteria' smart constructor.
newtype OriginGroupFailoverCriteria = OriginGroupFailoverCriteria'
  { statusCodes ::
      StatusCodes
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OriginGroupFailoverCriteria' with the minimum fields required to make a request.
--
-- * 'statusCodes' - The status codes that, when returned from the primary origin, will trigger CloudFront to failover to the second origin.
mkOriginGroupFailoverCriteria ::
  -- | 'statusCodes'
  StatusCodes ->
  OriginGroupFailoverCriteria
mkOriginGroupFailoverCriteria pStatusCodes_ =
  OriginGroupFailoverCriteria' {statusCodes = pStatusCodes_}

-- | The status codes that, when returned from the primary origin, will trigger CloudFront to failover to the second origin.
--
-- /Note:/ Consider using 'statusCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogfcStatusCodes :: Lens.Lens' OriginGroupFailoverCriteria StatusCodes
ogfcStatusCodes = Lens.lens (statusCodes :: OriginGroupFailoverCriteria -> StatusCodes) (\s a -> s {statusCodes = a} :: OriginGroupFailoverCriteria)
{-# DEPRECATED ogfcStatusCodes "Use generic-lens or generic-optics with 'statusCodes' instead." #-}

instance Lude.FromXML OriginGroupFailoverCriteria where
  parseXML x =
    OriginGroupFailoverCriteria' Lude.<$> (x Lude..@ "StatusCodes")

instance Lude.ToXML OriginGroupFailoverCriteria where
  toXML OriginGroupFailoverCriteria' {..} =
    Lude.mconcat ["StatusCodes" Lude.@= statusCodes]
