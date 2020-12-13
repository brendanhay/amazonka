{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectParentsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectParentsResponse
  ( BatchListObjectParentsResponse (..),

    -- * Smart constructor
    mkBatchListObjectParentsResponse,

    -- * Lenses
    blopNextToken,
    blopParentLinks,
  )
where

import Network.AWS.CloudDirectory.Types.ObjectIdentifierAndLinkNameTuple
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkBatchListObjectParentsResponse' smart constructor.
data BatchListObjectParentsResponse = BatchListObjectParentsResponse'
  { nextToken :: Lude.Maybe Lude.Text,
    parentLinks :: Lude.Maybe [ObjectIdentifierAndLinkNameTuple]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchListObjectParentsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' -
-- * 'parentLinks' -
mkBatchListObjectParentsResponse ::
  BatchListObjectParentsResponse
mkBatchListObjectParentsResponse =
  BatchListObjectParentsResponse'
    { nextToken = Lude.Nothing,
      parentLinks = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blopNextToken :: Lens.Lens' BatchListObjectParentsResponse (Lude.Maybe Lude.Text)
blopNextToken = Lens.lens (nextToken :: BatchListObjectParentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: BatchListObjectParentsResponse)
{-# DEPRECATED blopNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'parentLinks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blopParentLinks :: Lens.Lens' BatchListObjectParentsResponse (Lude.Maybe [ObjectIdentifierAndLinkNameTuple])
blopParentLinks = Lens.lens (parentLinks :: BatchListObjectParentsResponse -> Lude.Maybe [ObjectIdentifierAndLinkNameTuple]) (\s a -> s {parentLinks = a} :: BatchListObjectParentsResponse)
{-# DEPRECATED blopParentLinks "Use generic-lens or generic-optics with 'parentLinks' instead." #-}

instance Lude.FromJSON BatchListObjectParentsResponse where
  parseJSON =
    Lude.withObject
      "BatchListObjectParentsResponse"
      ( \x ->
          BatchListObjectParentsResponse'
            Lude.<$> (x Lude..:? "NextToken")
            Lude.<*> (x Lude..:? "ParentLinks" Lude..!= Lude.mempty)
      )
