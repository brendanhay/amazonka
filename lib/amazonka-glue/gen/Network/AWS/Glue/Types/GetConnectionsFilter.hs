{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.GetConnectionsFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.GetConnectionsFilter
  ( GetConnectionsFilter (..),

    -- * Smart constructor
    mkGetConnectionsFilter,

    -- * Lenses
    gcfMatchCriteria,
    gcfConnectionType,
  )
where

import Network.AWS.Glue.Types.ConnectionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Filters the connection definitions that are returned by the @GetConnections@ API operation.
--
-- /See:/ 'mkGetConnectionsFilter' smart constructor.
data GetConnectionsFilter = GetConnectionsFilter'
  { matchCriteria ::
      Lude.Maybe [Lude.Text],
    connectionType :: Lude.Maybe ConnectionType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetConnectionsFilter' with the minimum fields required to make a request.
--
-- * 'connectionType' - The type of connections to return. Currently, SFTP is not supported.
-- * 'matchCriteria' - A criteria string that must match the criteria recorded in the connection definition for that connection definition to be returned.
mkGetConnectionsFilter ::
  GetConnectionsFilter
mkGetConnectionsFilter =
  GetConnectionsFilter'
    { matchCriteria = Lude.Nothing,
      connectionType = Lude.Nothing
    }

-- | A criteria string that must match the criteria recorded in the connection definition for that connection definition to be returned.
--
-- /Note:/ Consider using 'matchCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfMatchCriteria :: Lens.Lens' GetConnectionsFilter (Lude.Maybe [Lude.Text])
gcfMatchCriteria = Lens.lens (matchCriteria :: GetConnectionsFilter -> Lude.Maybe [Lude.Text]) (\s a -> s {matchCriteria = a} :: GetConnectionsFilter)
{-# DEPRECATED gcfMatchCriteria "Use generic-lens or generic-optics with 'matchCriteria' instead." #-}

-- | The type of connections to return. Currently, SFTP is not supported.
--
-- /Note:/ Consider using 'connectionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfConnectionType :: Lens.Lens' GetConnectionsFilter (Lude.Maybe ConnectionType)
gcfConnectionType = Lens.lens (connectionType :: GetConnectionsFilter -> Lude.Maybe ConnectionType) (\s a -> s {connectionType = a} :: GetConnectionsFilter)
{-# DEPRECATED gcfConnectionType "Use generic-lens or generic-optics with 'connectionType' instead." #-}

instance Lude.ToJSON GetConnectionsFilter where
  toJSON GetConnectionsFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MatchCriteria" Lude..=) Lude.<$> matchCriteria,
            ("ConnectionType" Lude..=) Lude.<$> connectionType
          ]
      )
