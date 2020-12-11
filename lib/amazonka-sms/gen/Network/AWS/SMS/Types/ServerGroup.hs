-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerGroup
  ( ServerGroup (..),

    -- * Smart constructor
    mkServerGroup,

    -- * Lenses
    sgServerList,
    sgName,
    sgServerGroupId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SMS.Types.Server

-- | Logical grouping of servers.
--
-- /See:/ 'mkServerGroup' smart constructor.
data ServerGroup = ServerGroup'
  { serverList :: Lude.Maybe [Server],
    name :: Lude.Maybe Lude.Text,
    serverGroupId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServerGroup' with the minimum fields required to make a request.
--
-- * 'name' - The name of a server group.
-- * 'serverGroupId' - The ID of a server group.
-- * 'serverList' - The servers that belong to a server group.
mkServerGroup ::
  ServerGroup
mkServerGroup =
  ServerGroup'
    { serverList = Lude.Nothing,
      name = Lude.Nothing,
      serverGroupId = Lude.Nothing
    }

-- | The servers that belong to a server group.
--
-- /Note:/ Consider using 'serverList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgServerList :: Lens.Lens' ServerGroup (Lude.Maybe [Server])
sgServerList = Lens.lens (serverList :: ServerGroup -> Lude.Maybe [Server]) (\s a -> s {serverList = a} :: ServerGroup)
{-# DEPRECATED sgServerList "Use generic-lens or generic-optics with 'serverList' instead." #-}

-- | The name of a server group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgName :: Lens.Lens' ServerGroup (Lude.Maybe Lude.Text)
sgName = Lens.lens (name :: ServerGroup -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ServerGroup)
{-# DEPRECATED sgName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of a server group.
--
-- /Note:/ Consider using 'serverGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgServerGroupId :: Lens.Lens' ServerGroup (Lude.Maybe Lude.Text)
sgServerGroupId = Lens.lens (serverGroupId :: ServerGroup -> Lude.Maybe Lude.Text) (\s a -> s {serverGroupId = a} :: ServerGroup)
{-# DEPRECATED sgServerGroupId "Use generic-lens or generic-optics with 'serverGroupId' instead." #-}

instance Lude.FromJSON ServerGroup where
  parseJSON =
    Lude.withObject
      "ServerGroup"
      ( \x ->
          ServerGroup'
            Lude.<$> (x Lude..:? "serverList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "serverGroupId")
      )

instance Lude.ToJSON ServerGroup where
  toJSON ServerGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("serverList" Lude..=) Lude.<$> serverList,
            ("name" Lude..=) Lude.<$> name,
            ("serverGroupId" Lude..=) Lude.<$> serverGroupId
          ]
      )
