{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.UnprocessedUpdateAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.UnprocessedUpdateAction where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Update action that has failed to be processed for the corresponding
-- apply\/stop request
--
-- /See:/ 'newUnprocessedUpdateAction' smart constructor.
data UnprocessedUpdateAction = UnprocessedUpdateAction'
  { -- | The replication group ID
    replicationGroupId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the cache cluster
    cacheClusterId :: Prelude.Maybe Prelude.Text,
    -- | The error type for requests that are not processed
    errorType :: Prelude.Maybe Prelude.Text,
    -- | The error message that describes the reason the request was not
    -- processed
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the service update
    serviceUpdateName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UnprocessedUpdateAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationGroupId', 'unprocessedUpdateAction_replicationGroupId' - The replication group ID
--
-- 'cacheClusterId', 'unprocessedUpdateAction_cacheClusterId' - The ID of the cache cluster
--
-- 'errorType', 'unprocessedUpdateAction_errorType' - The error type for requests that are not processed
--
-- 'errorMessage', 'unprocessedUpdateAction_errorMessage' - The error message that describes the reason the request was not
-- processed
--
-- 'serviceUpdateName', 'unprocessedUpdateAction_serviceUpdateName' - The unique ID of the service update
newUnprocessedUpdateAction ::
  UnprocessedUpdateAction
newUnprocessedUpdateAction =
  UnprocessedUpdateAction'
    { replicationGroupId =
        Prelude.Nothing,
      cacheClusterId = Prelude.Nothing,
      errorType = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      serviceUpdateName = Prelude.Nothing
    }

-- | The replication group ID
unprocessedUpdateAction_replicationGroupId :: Lens.Lens' UnprocessedUpdateAction (Prelude.Maybe Prelude.Text)
unprocessedUpdateAction_replicationGroupId = Lens.lens (\UnprocessedUpdateAction' {replicationGroupId} -> replicationGroupId) (\s@UnprocessedUpdateAction' {} a -> s {replicationGroupId = a} :: UnprocessedUpdateAction)

-- | The ID of the cache cluster
unprocessedUpdateAction_cacheClusterId :: Lens.Lens' UnprocessedUpdateAction (Prelude.Maybe Prelude.Text)
unprocessedUpdateAction_cacheClusterId = Lens.lens (\UnprocessedUpdateAction' {cacheClusterId} -> cacheClusterId) (\s@UnprocessedUpdateAction' {} a -> s {cacheClusterId = a} :: UnprocessedUpdateAction)

-- | The error type for requests that are not processed
unprocessedUpdateAction_errorType :: Lens.Lens' UnprocessedUpdateAction (Prelude.Maybe Prelude.Text)
unprocessedUpdateAction_errorType = Lens.lens (\UnprocessedUpdateAction' {errorType} -> errorType) (\s@UnprocessedUpdateAction' {} a -> s {errorType = a} :: UnprocessedUpdateAction)

-- | The error message that describes the reason the request was not
-- processed
unprocessedUpdateAction_errorMessage :: Lens.Lens' UnprocessedUpdateAction (Prelude.Maybe Prelude.Text)
unprocessedUpdateAction_errorMessage = Lens.lens (\UnprocessedUpdateAction' {errorMessage} -> errorMessage) (\s@UnprocessedUpdateAction' {} a -> s {errorMessage = a} :: UnprocessedUpdateAction)

-- | The unique ID of the service update
unprocessedUpdateAction_serviceUpdateName :: Lens.Lens' UnprocessedUpdateAction (Prelude.Maybe Prelude.Text)
unprocessedUpdateAction_serviceUpdateName = Lens.lens (\UnprocessedUpdateAction' {serviceUpdateName} -> serviceUpdateName) (\s@UnprocessedUpdateAction' {} a -> s {serviceUpdateName = a} :: UnprocessedUpdateAction)

instance Prelude.FromXML UnprocessedUpdateAction where
  parseXML x =
    UnprocessedUpdateAction'
      Prelude.<$> (x Prelude..@? "ReplicationGroupId")
      Prelude.<*> (x Prelude..@? "CacheClusterId")
      Prelude.<*> (x Prelude..@? "ErrorType")
      Prelude.<*> (x Prelude..@? "ErrorMessage")
      Prelude.<*> (x Prelude..@? "ServiceUpdateName")

instance Prelude.Hashable UnprocessedUpdateAction

instance Prelude.NFData UnprocessedUpdateAction
