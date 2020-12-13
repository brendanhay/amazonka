{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateGlobalCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Aurora global database spread across multiple AWS Regions. The global database contains a single primary cluster with read-write capability, and a read-only secondary cluster that receives data from the primary cluster through high-speed replication performed by the Aurora storage subsystem.
--
-- You can create a global database that is initially empty, and then add a primary cluster and a secondary cluster to it. Or you can specify an existing Aurora cluster during the create operation, and this cluster becomes the primary cluster of the global database.
module Network.AWS.RDS.CreateGlobalCluster
  ( -- * Creating a request
    CreateGlobalCluster (..),
    mkCreateGlobalCluster,

    -- ** Request lenses
    cgcEngineVersion,
    cgcDeletionProtection,
    cgcStorageEncrypted,
    cgcSourceDBClusterIdentifier,
    cgcGlobalClusterIdentifier,
    cgcEngine,
    cgcDatabaseName,

    -- * Destructuring the response
    CreateGlobalClusterResponse (..),
    mkCreateGlobalClusterResponse,

    -- ** Response lenses
    cgcrsGlobalCluster,
    cgcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateGlobalCluster' smart constructor.
data CreateGlobalCluster = CreateGlobalCluster'
  { -- | The engine version of the Aurora global database.
    engineVersion :: Lude.Maybe Lude.Text,
    -- | The deletion protection setting for the new global database. The global database can't be deleted when deletion protection is enabled.
    deletionProtection :: Lude.Maybe Lude.Bool,
    -- | The storage encryption setting for the new global database cluster.
    storageEncrypted :: Lude.Maybe Lude.Bool,
    -- | The Amazon Resource Name (ARN) to use as the primary cluster of the global database. This parameter is optional.
    sourceDBClusterIdentifier :: Lude.Maybe Lude.Text,
    -- | The cluster identifier of the new global database cluster.
    globalClusterIdentifier :: Lude.Maybe Lude.Text,
    -- | The name of the database engine to be used for this DB cluster.
    engine :: Lude.Maybe Lude.Text,
    -- | The name for your database of up to 64 alpha-numeric characters. If you do not provide a name, Amazon Aurora will not create a database in the global database cluster you are creating.
    databaseName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGlobalCluster' with the minimum fields required to make a request.
--
-- * 'engineVersion' - The engine version of the Aurora global database.
-- * 'deletionProtection' - The deletion protection setting for the new global database. The global database can't be deleted when deletion protection is enabled.
-- * 'storageEncrypted' - The storage encryption setting for the new global database cluster.
-- * 'sourceDBClusterIdentifier' - The Amazon Resource Name (ARN) to use as the primary cluster of the global database. This parameter is optional.
-- * 'globalClusterIdentifier' - The cluster identifier of the new global database cluster.
-- * 'engine' - The name of the database engine to be used for this DB cluster.
-- * 'databaseName' - The name for your database of up to 64 alpha-numeric characters. If you do not provide a name, Amazon Aurora will not create a database in the global database cluster you are creating.
mkCreateGlobalCluster ::
  CreateGlobalCluster
mkCreateGlobalCluster =
  CreateGlobalCluster'
    { engineVersion = Lude.Nothing,
      deletionProtection = Lude.Nothing,
      storageEncrypted = Lude.Nothing,
      sourceDBClusterIdentifier = Lude.Nothing,
      globalClusterIdentifier = Lude.Nothing,
      engine = Lude.Nothing,
      databaseName = Lude.Nothing
    }

-- | The engine version of the Aurora global database.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcEngineVersion :: Lens.Lens' CreateGlobalCluster (Lude.Maybe Lude.Text)
cgcEngineVersion = Lens.lens (engineVersion :: CreateGlobalCluster -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: CreateGlobalCluster)
{-# DEPRECATED cgcEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The deletion protection setting for the new global database. The global database can't be deleted when deletion protection is enabled.
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcDeletionProtection :: Lens.Lens' CreateGlobalCluster (Lude.Maybe Lude.Bool)
cgcDeletionProtection = Lens.lens (deletionProtection :: CreateGlobalCluster -> Lude.Maybe Lude.Bool) (\s a -> s {deletionProtection = a} :: CreateGlobalCluster)
{-# DEPRECATED cgcDeletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead." #-}

-- | The storage encryption setting for the new global database cluster.
--
-- /Note:/ Consider using 'storageEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcStorageEncrypted :: Lens.Lens' CreateGlobalCluster (Lude.Maybe Lude.Bool)
cgcStorageEncrypted = Lens.lens (storageEncrypted :: CreateGlobalCluster -> Lude.Maybe Lude.Bool) (\s a -> s {storageEncrypted = a} :: CreateGlobalCluster)
{-# DEPRECATED cgcStorageEncrypted "Use generic-lens or generic-optics with 'storageEncrypted' instead." #-}

-- | The Amazon Resource Name (ARN) to use as the primary cluster of the global database. This parameter is optional.
--
-- /Note:/ Consider using 'sourceDBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcSourceDBClusterIdentifier :: Lens.Lens' CreateGlobalCluster (Lude.Maybe Lude.Text)
cgcSourceDBClusterIdentifier = Lens.lens (sourceDBClusterIdentifier :: CreateGlobalCluster -> Lude.Maybe Lude.Text) (\s a -> s {sourceDBClusterIdentifier = a} :: CreateGlobalCluster)
{-# DEPRECATED cgcSourceDBClusterIdentifier "Use generic-lens or generic-optics with 'sourceDBClusterIdentifier' instead." #-}

-- | The cluster identifier of the new global database cluster.
--
-- /Note:/ Consider using 'globalClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcGlobalClusterIdentifier :: Lens.Lens' CreateGlobalCluster (Lude.Maybe Lude.Text)
cgcGlobalClusterIdentifier = Lens.lens (globalClusterIdentifier :: CreateGlobalCluster -> Lude.Maybe Lude.Text) (\s a -> s {globalClusterIdentifier = a} :: CreateGlobalCluster)
{-# DEPRECATED cgcGlobalClusterIdentifier "Use generic-lens or generic-optics with 'globalClusterIdentifier' instead." #-}

-- | The name of the database engine to be used for this DB cluster.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcEngine :: Lens.Lens' CreateGlobalCluster (Lude.Maybe Lude.Text)
cgcEngine = Lens.lens (engine :: CreateGlobalCluster -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: CreateGlobalCluster)
{-# DEPRECATED cgcEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The name for your database of up to 64 alpha-numeric characters. If you do not provide a name, Amazon Aurora will not create a database in the global database cluster you are creating.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcDatabaseName :: Lens.Lens' CreateGlobalCluster (Lude.Maybe Lude.Text)
cgcDatabaseName = Lens.lens (databaseName :: CreateGlobalCluster -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: CreateGlobalCluster)
{-# DEPRECATED cgcDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

instance Lude.AWSRequest CreateGlobalCluster where
  type Rs CreateGlobalCluster = CreateGlobalClusterResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "CreateGlobalClusterResult"
      ( \s h x ->
          CreateGlobalClusterResponse'
            Lude.<$> (x Lude..@? "GlobalCluster")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateGlobalCluster where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateGlobalCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateGlobalCluster where
  toQuery CreateGlobalCluster' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateGlobalCluster" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "EngineVersion" Lude.=: engineVersion,
        "DeletionProtection" Lude.=: deletionProtection,
        "StorageEncrypted" Lude.=: storageEncrypted,
        "SourceDBClusterIdentifier" Lude.=: sourceDBClusterIdentifier,
        "GlobalClusterIdentifier" Lude.=: globalClusterIdentifier,
        "Engine" Lude.=: engine,
        "DatabaseName" Lude.=: databaseName
      ]

-- | /See:/ 'mkCreateGlobalClusterResponse' smart constructor.
data CreateGlobalClusterResponse = CreateGlobalClusterResponse'
  { globalCluster :: Lude.Maybe GlobalCluster,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGlobalClusterResponse' with the minimum fields required to make a request.
--
-- * 'globalCluster' -
-- * 'responseStatus' - The response status code.
mkCreateGlobalClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateGlobalClusterResponse
mkCreateGlobalClusterResponse pResponseStatus_ =
  CreateGlobalClusterResponse'
    { globalCluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcrsGlobalCluster :: Lens.Lens' CreateGlobalClusterResponse (Lude.Maybe GlobalCluster)
cgcrsGlobalCluster = Lens.lens (globalCluster :: CreateGlobalClusterResponse -> Lude.Maybe GlobalCluster) (\s a -> s {globalCluster = a} :: CreateGlobalClusterResponse)
{-# DEPRECATED cgcrsGlobalCluster "Use generic-lens or generic-optics with 'globalCluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcrsResponseStatus :: Lens.Lens' CreateGlobalClusterResponse Lude.Int
cgcrsResponseStatus = Lens.lens (responseStatus :: CreateGlobalClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateGlobalClusterResponse)
{-# DEPRECATED cgcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
