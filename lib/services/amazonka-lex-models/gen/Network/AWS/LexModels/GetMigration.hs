{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetMigration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides details about an ongoing or complete migration from an Amazon
-- Lex V1 bot to an Amazon Lex V2 bot. Use this operation to view the
-- migration alerts and warnings related to the migration.
module Network.AWS.LexModels.GetMigration
  ( -- * Creating a Request
    GetMigration (..),
    newGetMigration,

    -- * Request Lenses
    getMigration_migrationId,

    -- * Destructuring the Response
    GetMigrationResponse (..),
    newGetMigrationResponse,

    -- * Response Lenses
    getMigrationResponse_v1BotVersion,
    getMigrationResponse_migrationId,
    getMigrationResponse_v1BotLocale,
    getMigrationResponse_migrationTimestamp,
    getMigrationResponse_migrationStrategy,
    getMigrationResponse_v2BotRole,
    getMigrationResponse_v1BotName,
    getMigrationResponse_v2BotId,
    getMigrationResponse_migrationStatus,
    getMigrationResponse_alerts,
    getMigrationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetMigration' smart constructor.
data GetMigration = GetMigration'
  { -- | The unique identifier of the migration to view. The @migrationID@ is
    -- returned by the operation.
    migrationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMigration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'migrationId', 'getMigration_migrationId' - The unique identifier of the migration to view. The @migrationID@ is
-- returned by the operation.
newGetMigration ::
  -- | 'migrationId'
  Prelude.Text ->
  GetMigration
newGetMigration pMigrationId_ =
  GetMigration' {migrationId = pMigrationId_}

-- | The unique identifier of the migration to view. The @migrationID@ is
-- returned by the operation.
getMigration_migrationId :: Lens.Lens' GetMigration Prelude.Text
getMigration_migrationId = Lens.lens (\GetMigration' {migrationId} -> migrationId) (\s@GetMigration' {} a -> s {migrationId = a} :: GetMigration)

instance Core.AWSRequest GetMigration where
  type AWSResponse GetMigration = GetMigrationResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMigrationResponse'
            Prelude.<$> (x Core..?> "v1BotVersion")
            Prelude.<*> (x Core..?> "migrationId")
            Prelude.<*> (x Core..?> "v1BotLocale")
            Prelude.<*> (x Core..?> "migrationTimestamp")
            Prelude.<*> (x Core..?> "migrationStrategy")
            Prelude.<*> (x Core..?> "v2BotRole")
            Prelude.<*> (x Core..?> "v1BotName")
            Prelude.<*> (x Core..?> "v2BotId")
            Prelude.<*> (x Core..?> "migrationStatus")
            Prelude.<*> (x Core..?> "alerts" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMigration

instance Prelude.NFData GetMigration

instance Core.ToHeaders GetMigration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetMigration where
  toPath GetMigration' {..} =
    Prelude.mconcat
      ["/migrations/", Core.toBS migrationId]

instance Core.ToQuery GetMigration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMigrationResponse' smart constructor.
data GetMigrationResponse = GetMigrationResponse'
  { -- | The version of the Amazon Lex V1 bot migrated to Amazon Lex V2.
    v1BotVersion :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the migration. This is the same as the
    -- identifier used when calling the @GetMigration@ operation.
    migrationId :: Prelude.Maybe Prelude.Text,
    -- | The locale of the Amazon Lex V1 bot migrated to Amazon Lex V2.
    v1BotLocale :: Prelude.Maybe Locale,
    -- | The date and time that the migration started.
    migrationTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The strategy used to conduct the migration.
    --
    -- -   @CREATE_NEW@ - Creates a new Amazon Lex V2 bot and migrates the
    --     Amazon Lex V1 bot to the new bot.
    --
    -- -   @UPDATE_EXISTING@ - Overwrites the existing Amazon Lex V2 bot
    --     metadata and the locale being migrated. It doesn\'t change any other
    --     locales in the Amazon Lex V2 bot. If the locale doesn\'t exist, a
    --     new locale is created in the Amazon Lex V2 bot.
    migrationStrategy :: Prelude.Maybe MigrationStrategy,
    -- | The IAM role that Amazon Lex uses to run the Amazon Lex V2 bot.
    v2BotRole :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon Lex V1 bot migrated to Amazon Lex V2.
    v1BotName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the Amazon Lex V2 bot that the Amazon Lex V1 is
    -- being migrated to.
    v2BotId :: Prelude.Maybe Prelude.Text,
    -- | Indicates the status of the migration. When the status is @COMPLETE@ the
    -- migration is finished and the bot is available in Amazon Lex V2. There
    -- may be alerts and warnings that need to be resolved to complete the
    -- migration.
    migrationStatus :: Prelude.Maybe MigrationStatus,
    -- | A list of alerts and warnings that indicate issues with the migration
    -- for the Amazon Lex V1 bot to Amazon Lex V2. You receive a warning when
    -- an Amazon Lex V1 feature has a different implementation if Amazon Lex
    -- V2.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/migrate.html Migrating a bot>
    -- in the /Amazon Lex V2 developer guide/.
    alerts :: Prelude.Maybe [MigrationAlert],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMigrationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'v1BotVersion', 'getMigrationResponse_v1BotVersion' - The version of the Amazon Lex V1 bot migrated to Amazon Lex V2.
--
-- 'migrationId', 'getMigrationResponse_migrationId' - The unique identifier of the migration. This is the same as the
-- identifier used when calling the @GetMigration@ operation.
--
-- 'v1BotLocale', 'getMigrationResponse_v1BotLocale' - The locale of the Amazon Lex V1 bot migrated to Amazon Lex V2.
--
-- 'migrationTimestamp', 'getMigrationResponse_migrationTimestamp' - The date and time that the migration started.
--
-- 'migrationStrategy', 'getMigrationResponse_migrationStrategy' - The strategy used to conduct the migration.
--
-- -   @CREATE_NEW@ - Creates a new Amazon Lex V2 bot and migrates the
--     Amazon Lex V1 bot to the new bot.
--
-- -   @UPDATE_EXISTING@ - Overwrites the existing Amazon Lex V2 bot
--     metadata and the locale being migrated. It doesn\'t change any other
--     locales in the Amazon Lex V2 bot. If the locale doesn\'t exist, a
--     new locale is created in the Amazon Lex V2 bot.
--
-- 'v2BotRole', 'getMigrationResponse_v2BotRole' - The IAM role that Amazon Lex uses to run the Amazon Lex V2 bot.
--
-- 'v1BotName', 'getMigrationResponse_v1BotName' - The name of the Amazon Lex V1 bot migrated to Amazon Lex V2.
--
-- 'v2BotId', 'getMigrationResponse_v2BotId' - The unique identifier of the Amazon Lex V2 bot that the Amazon Lex V1 is
-- being migrated to.
--
-- 'migrationStatus', 'getMigrationResponse_migrationStatus' - Indicates the status of the migration. When the status is @COMPLETE@ the
-- migration is finished and the bot is available in Amazon Lex V2. There
-- may be alerts and warnings that need to be resolved to complete the
-- migration.
--
-- 'alerts', 'getMigrationResponse_alerts' - A list of alerts and warnings that indicate issues with the migration
-- for the Amazon Lex V1 bot to Amazon Lex V2. You receive a warning when
-- an Amazon Lex V1 feature has a different implementation if Amazon Lex
-- V2.
--
-- For more information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/migrate.html Migrating a bot>
-- in the /Amazon Lex V2 developer guide/.
--
-- 'httpStatus', 'getMigrationResponse_httpStatus' - The response's http status code.
newGetMigrationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMigrationResponse
newGetMigrationResponse pHttpStatus_ =
  GetMigrationResponse'
    { v1BotVersion =
        Prelude.Nothing,
      migrationId = Prelude.Nothing,
      v1BotLocale = Prelude.Nothing,
      migrationTimestamp = Prelude.Nothing,
      migrationStrategy = Prelude.Nothing,
      v2BotRole = Prelude.Nothing,
      v1BotName = Prelude.Nothing,
      v2BotId = Prelude.Nothing,
      migrationStatus = Prelude.Nothing,
      alerts = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version of the Amazon Lex V1 bot migrated to Amazon Lex V2.
getMigrationResponse_v1BotVersion :: Lens.Lens' GetMigrationResponse (Prelude.Maybe Prelude.Text)
getMigrationResponse_v1BotVersion = Lens.lens (\GetMigrationResponse' {v1BotVersion} -> v1BotVersion) (\s@GetMigrationResponse' {} a -> s {v1BotVersion = a} :: GetMigrationResponse)

-- | The unique identifier of the migration. This is the same as the
-- identifier used when calling the @GetMigration@ operation.
getMigrationResponse_migrationId :: Lens.Lens' GetMigrationResponse (Prelude.Maybe Prelude.Text)
getMigrationResponse_migrationId = Lens.lens (\GetMigrationResponse' {migrationId} -> migrationId) (\s@GetMigrationResponse' {} a -> s {migrationId = a} :: GetMigrationResponse)

-- | The locale of the Amazon Lex V1 bot migrated to Amazon Lex V2.
getMigrationResponse_v1BotLocale :: Lens.Lens' GetMigrationResponse (Prelude.Maybe Locale)
getMigrationResponse_v1BotLocale = Lens.lens (\GetMigrationResponse' {v1BotLocale} -> v1BotLocale) (\s@GetMigrationResponse' {} a -> s {v1BotLocale = a} :: GetMigrationResponse)

-- | The date and time that the migration started.
getMigrationResponse_migrationTimestamp :: Lens.Lens' GetMigrationResponse (Prelude.Maybe Prelude.UTCTime)
getMigrationResponse_migrationTimestamp = Lens.lens (\GetMigrationResponse' {migrationTimestamp} -> migrationTimestamp) (\s@GetMigrationResponse' {} a -> s {migrationTimestamp = a} :: GetMigrationResponse) Prelude.. Lens.mapping Core._Time

-- | The strategy used to conduct the migration.
--
-- -   @CREATE_NEW@ - Creates a new Amazon Lex V2 bot and migrates the
--     Amazon Lex V1 bot to the new bot.
--
-- -   @UPDATE_EXISTING@ - Overwrites the existing Amazon Lex V2 bot
--     metadata and the locale being migrated. It doesn\'t change any other
--     locales in the Amazon Lex V2 bot. If the locale doesn\'t exist, a
--     new locale is created in the Amazon Lex V2 bot.
getMigrationResponse_migrationStrategy :: Lens.Lens' GetMigrationResponse (Prelude.Maybe MigrationStrategy)
getMigrationResponse_migrationStrategy = Lens.lens (\GetMigrationResponse' {migrationStrategy} -> migrationStrategy) (\s@GetMigrationResponse' {} a -> s {migrationStrategy = a} :: GetMigrationResponse)

-- | The IAM role that Amazon Lex uses to run the Amazon Lex V2 bot.
getMigrationResponse_v2BotRole :: Lens.Lens' GetMigrationResponse (Prelude.Maybe Prelude.Text)
getMigrationResponse_v2BotRole = Lens.lens (\GetMigrationResponse' {v2BotRole} -> v2BotRole) (\s@GetMigrationResponse' {} a -> s {v2BotRole = a} :: GetMigrationResponse)

-- | The name of the Amazon Lex V1 bot migrated to Amazon Lex V2.
getMigrationResponse_v1BotName :: Lens.Lens' GetMigrationResponse (Prelude.Maybe Prelude.Text)
getMigrationResponse_v1BotName = Lens.lens (\GetMigrationResponse' {v1BotName} -> v1BotName) (\s@GetMigrationResponse' {} a -> s {v1BotName = a} :: GetMigrationResponse)

-- | The unique identifier of the Amazon Lex V2 bot that the Amazon Lex V1 is
-- being migrated to.
getMigrationResponse_v2BotId :: Lens.Lens' GetMigrationResponse (Prelude.Maybe Prelude.Text)
getMigrationResponse_v2BotId = Lens.lens (\GetMigrationResponse' {v2BotId} -> v2BotId) (\s@GetMigrationResponse' {} a -> s {v2BotId = a} :: GetMigrationResponse)

-- | Indicates the status of the migration. When the status is @COMPLETE@ the
-- migration is finished and the bot is available in Amazon Lex V2. There
-- may be alerts and warnings that need to be resolved to complete the
-- migration.
getMigrationResponse_migrationStatus :: Lens.Lens' GetMigrationResponse (Prelude.Maybe MigrationStatus)
getMigrationResponse_migrationStatus = Lens.lens (\GetMigrationResponse' {migrationStatus} -> migrationStatus) (\s@GetMigrationResponse' {} a -> s {migrationStatus = a} :: GetMigrationResponse)

-- | A list of alerts and warnings that indicate issues with the migration
-- for the Amazon Lex V1 bot to Amazon Lex V2. You receive a warning when
-- an Amazon Lex V1 feature has a different implementation if Amazon Lex
-- V2.
--
-- For more information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/migrate.html Migrating a bot>
-- in the /Amazon Lex V2 developer guide/.
getMigrationResponse_alerts :: Lens.Lens' GetMigrationResponse (Prelude.Maybe [MigrationAlert])
getMigrationResponse_alerts = Lens.lens (\GetMigrationResponse' {alerts} -> alerts) (\s@GetMigrationResponse' {} a -> s {alerts = a} :: GetMigrationResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getMigrationResponse_httpStatus :: Lens.Lens' GetMigrationResponse Prelude.Int
getMigrationResponse_httpStatus = Lens.lens (\GetMigrationResponse' {httpStatus} -> httpStatus) (\s@GetMigrationResponse' {} a -> s {httpStatus = a} :: GetMigrationResponse)

instance Prelude.NFData GetMigrationResponse
