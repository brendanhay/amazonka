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
-- Module      : Amazonka.LexModels.StartMigration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts migrating a bot from Amazon Lex V1 to Amazon Lex V2. Migrate your
-- bot when you want to take advantage of the new features of Amazon Lex
-- V2.
--
-- For more information, see
-- <https://docs.aws.amazon.com/lex/latest/dg/migrate.html Migrating a bot>
-- in the /Amazon Lex developer guide/.
module Amazonka.LexModels.StartMigration
  ( -- * Creating a Request
    StartMigration (..),
    newStartMigration,

    -- * Request Lenses
    startMigration_v1BotName,
    startMigration_v1BotVersion,
    startMigration_v2BotName,
    startMigration_v2BotRole,
    startMigration_migrationStrategy,

    -- * Destructuring the Response
    StartMigrationResponse (..),
    newStartMigrationResponse,

    -- * Response Lenses
    startMigrationResponse_migrationId,
    startMigrationResponse_migrationStrategy,
    startMigrationResponse_migrationTimestamp,
    startMigrationResponse_v1BotLocale,
    startMigrationResponse_v1BotName,
    startMigrationResponse_v1BotVersion,
    startMigrationResponse_v2BotId,
    startMigrationResponse_v2BotRole,
    startMigrationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartMigration' smart constructor.
data StartMigration = StartMigration'
  { -- | The name of the Amazon Lex V1 bot that you are migrating to Amazon Lex
    -- V2.
    v1BotName :: Prelude.Text,
    -- | The version of the bot to migrate to Amazon Lex V2. You can migrate the
    -- @$LATEST@ version as well as any numbered version.
    v1BotVersion :: Prelude.Text,
    -- | The name of the Amazon Lex V2 bot that you are migrating the Amazon Lex
    -- V1 bot to.
    --
    -- -   If the Amazon Lex V2 bot doesn\'t exist, you must use the
    --     @CREATE_NEW@ migration strategy.
    --
    -- -   If the Amazon Lex V2 bot exists, you must use the @UPDATE_EXISTING@
    --     migration strategy to change the contents of the Amazon Lex V2 bot.
    v2BotName :: Prelude.Text,
    -- | The IAM role that Amazon Lex uses to run the Amazon Lex V2 bot.
    v2BotRole :: Prelude.Text,
    -- | The strategy used to conduct the migration.
    --
    -- -   @CREATE_NEW@ - Creates a new Amazon Lex V2 bot and migrates the
    --     Amazon Lex V1 bot to the new bot.
    --
    -- -   @UPDATE_EXISTING@ - Overwrites the existing Amazon Lex V2 bot
    --     metadata and the locale being migrated. It doesn\'t change any other
    --     locales in the Amazon Lex V2 bot. If the locale doesn\'t exist, a
    --     new locale is created in the Amazon Lex V2 bot.
    migrationStrategy :: MigrationStrategy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartMigration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'v1BotName', 'startMigration_v1BotName' - The name of the Amazon Lex V1 bot that you are migrating to Amazon Lex
-- V2.
--
-- 'v1BotVersion', 'startMigration_v1BotVersion' - The version of the bot to migrate to Amazon Lex V2. You can migrate the
-- @$LATEST@ version as well as any numbered version.
--
-- 'v2BotName', 'startMigration_v2BotName' - The name of the Amazon Lex V2 bot that you are migrating the Amazon Lex
-- V1 bot to.
--
-- -   If the Amazon Lex V2 bot doesn\'t exist, you must use the
--     @CREATE_NEW@ migration strategy.
--
-- -   If the Amazon Lex V2 bot exists, you must use the @UPDATE_EXISTING@
--     migration strategy to change the contents of the Amazon Lex V2 bot.
--
-- 'v2BotRole', 'startMigration_v2BotRole' - The IAM role that Amazon Lex uses to run the Amazon Lex V2 bot.
--
-- 'migrationStrategy', 'startMigration_migrationStrategy' - The strategy used to conduct the migration.
--
-- -   @CREATE_NEW@ - Creates a new Amazon Lex V2 bot and migrates the
--     Amazon Lex V1 bot to the new bot.
--
-- -   @UPDATE_EXISTING@ - Overwrites the existing Amazon Lex V2 bot
--     metadata and the locale being migrated. It doesn\'t change any other
--     locales in the Amazon Lex V2 bot. If the locale doesn\'t exist, a
--     new locale is created in the Amazon Lex V2 bot.
newStartMigration ::
  -- | 'v1BotName'
  Prelude.Text ->
  -- | 'v1BotVersion'
  Prelude.Text ->
  -- | 'v2BotName'
  Prelude.Text ->
  -- | 'v2BotRole'
  Prelude.Text ->
  -- | 'migrationStrategy'
  MigrationStrategy ->
  StartMigration
newStartMigration
  pV1BotName_
  pV1BotVersion_
  pV2BotName_
  pV2BotRole_
  pMigrationStrategy_ =
    StartMigration'
      { v1BotName = pV1BotName_,
        v1BotVersion = pV1BotVersion_,
        v2BotName = pV2BotName_,
        v2BotRole = pV2BotRole_,
        migrationStrategy = pMigrationStrategy_
      }

-- | The name of the Amazon Lex V1 bot that you are migrating to Amazon Lex
-- V2.
startMigration_v1BotName :: Lens.Lens' StartMigration Prelude.Text
startMigration_v1BotName = Lens.lens (\StartMigration' {v1BotName} -> v1BotName) (\s@StartMigration' {} a -> s {v1BotName = a} :: StartMigration)

-- | The version of the bot to migrate to Amazon Lex V2. You can migrate the
-- @$LATEST@ version as well as any numbered version.
startMigration_v1BotVersion :: Lens.Lens' StartMigration Prelude.Text
startMigration_v1BotVersion = Lens.lens (\StartMigration' {v1BotVersion} -> v1BotVersion) (\s@StartMigration' {} a -> s {v1BotVersion = a} :: StartMigration)

-- | The name of the Amazon Lex V2 bot that you are migrating the Amazon Lex
-- V1 bot to.
--
-- -   If the Amazon Lex V2 bot doesn\'t exist, you must use the
--     @CREATE_NEW@ migration strategy.
--
-- -   If the Amazon Lex V2 bot exists, you must use the @UPDATE_EXISTING@
--     migration strategy to change the contents of the Amazon Lex V2 bot.
startMigration_v2BotName :: Lens.Lens' StartMigration Prelude.Text
startMigration_v2BotName = Lens.lens (\StartMigration' {v2BotName} -> v2BotName) (\s@StartMigration' {} a -> s {v2BotName = a} :: StartMigration)

-- | The IAM role that Amazon Lex uses to run the Amazon Lex V2 bot.
startMigration_v2BotRole :: Lens.Lens' StartMigration Prelude.Text
startMigration_v2BotRole = Lens.lens (\StartMigration' {v2BotRole} -> v2BotRole) (\s@StartMigration' {} a -> s {v2BotRole = a} :: StartMigration)

-- | The strategy used to conduct the migration.
--
-- -   @CREATE_NEW@ - Creates a new Amazon Lex V2 bot and migrates the
--     Amazon Lex V1 bot to the new bot.
--
-- -   @UPDATE_EXISTING@ - Overwrites the existing Amazon Lex V2 bot
--     metadata and the locale being migrated. It doesn\'t change any other
--     locales in the Amazon Lex V2 bot. If the locale doesn\'t exist, a
--     new locale is created in the Amazon Lex V2 bot.
startMigration_migrationStrategy :: Lens.Lens' StartMigration MigrationStrategy
startMigration_migrationStrategy = Lens.lens (\StartMigration' {migrationStrategy} -> migrationStrategy) (\s@StartMigration' {} a -> s {migrationStrategy = a} :: StartMigration)

instance Core.AWSRequest StartMigration where
  type
    AWSResponse StartMigration =
      StartMigrationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartMigrationResponse'
            Prelude.<$> (x Data..?> "migrationId")
            Prelude.<*> (x Data..?> "migrationStrategy")
            Prelude.<*> (x Data..?> "migrationTimestamp")
            Prelude.<*> (x Data..?> "v1BotLocale")
            Prelude.<*> (x Data..?> "v1BotName")
            Prelude.<*> (x Data..?> "v1BotVersion")
            Prelude.<*> (x Data..?> "v2BotId")
            Prelude.<*> (x Data..?> "v2BotRole")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartMigration where
  hashWithSalt _salt StartMigration' {..} =
    _salt
      `Prelude.hashWithSalt` v1BotName
      `Prelude.hashWithSalt` v1BotVersion
      `Prelude.hashWithSalt` v2BotName
      `Prelude.hashWithSalt` v2BotRole
      `Prelude.hashWithSalt` migrationStrategy

instance Prelude.NFData StartMigration where
  rnf StartMigration' {..} =
    Prelude.rnf v1BotName
      `Prelude.seq` Prelude.rnf v1BotVersion
      `Prelude.seq` Prelude.rnf v2BotName
      `Prelude.seq` Prelude.rnf v2BotRole
      `Prelude.seq` Prelude.rnf migrationStrategy

instance Data.ToHeaders StartMigration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartMigration where
  toJSON StartMigration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("v1BotName" Data..= v1BotName),
            Prelude.Just ("v1BotVersion" Data..= v1BotVersion),
            Prelude.Just ("v2BotName" Data..= v2BotName),
            Prelude.Just ("v2BotRole" Data..= v2BotRole),
            Prelude.Just
              ("migrationStrategy" Data..= migrationStrategy)
          ]
      )

instance Data.ToPath StartMigration where
  toPath = Prelude.const "/migrations"

instance Data.ToQuery StartMigration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartMigrationResponse' smart constructor.
data StartMigrationResponse = StartMigrationResponse'
  { -- | The unique identifier that Amazon Lex assigned to the migration.
    migrationId :: Prelude.Maybe Prelude.Text,
    -- | The strategy used to conduct the migration.
    migrationStrategy :: Prelude.Maybe MigrationStrategy,
    -- | The date and time that the migration started.
    migrationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The locale used for the Amazon Lex V1 bot.
    v1BotLocale :: Prelude.Maybe Locale,
    -- | The name of the Amazon Lex V1 bot that you are migrating to Amazon Lex
    -- V2.
    v1BotName :: Prelude.Maybe Prelude.Text,
    -- | The version of the bot to migrate to Amazon Lex V2.
    v1BotVersion :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the Amazon Lex V2 bot.
    v2BotId :: Prelude.Maybe Prelude.Text,
    -- | The IAM role that Amazon Lex uses to run the Amazon Lex V2 bot.
    v2BotRole :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartMigrationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'migrationId', 'startMigrationResponse_migrationId' - The unique identifier that Amazon Lex assigned to the migration.
--
-- 'migrationStrategy', 'startMigrationResponse_migrationStrategy' - The strategy used to conduct the migration.
--
-- 'migrationTimestamp', 'startMigrationResponse_migrationTimestamp' - The date and time that the migration started.
--
-- 'v1BotLocale', 'startMigrationResponse_v1BotLocale' - The locale used for the Amazon Lex V1 bot.
--
-- 'v1BotName', 'startMigrationResponse_v1BotName' - The name of the Amazon Lex V1 bot that you are migrating to Amazon Lex
-- V2.
--
-- 'v1BotVersion', 'startMigrationResponse_v1BotVersion' - The version of the bot to migrate to Amazon Lex V2.
--
-- 'v2BotId', 'startMigrationResponse_v2BotId' - The unique identifier for the Amazon Lex V2 bot.
--
-- 'v2BotRole', 'startMigrationResponse_v2BotRole' - The IAM role that Amazon Lex uses to run the Amazon Lex V2 bot.
--
-- 'httpStatus', 'startMigrationResponse_httpStatus' - The response's http status code.
newStartMigrationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartMigrationResponse
newStartMigrationResponse pHttpStatus_ =
  StartMigrationResponse'
    { migrationId =
        Prelude.Nothing,
      migrationStrategy = Prelude.Nothing,
      migrationTimestamp = Prelude.Nothing,
      v1BotLocale = Prelude.Nothing,
      v1BotName = Prelude.Nothing,
      v1BotVersion = Prelude.Nothing,
      v2BotId = Prelude.Nothing,
      v2BotRole = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier that Amazon Lex assigned to the migration.
startMigrationResponse_migrationId :: Lens.Lens' StartMigrationResponse (Prelude.Maybe Prelude.Text)
startMigrationResponse_migrationId = Lens.lens (\StartMigrationResponse' {migrationId} -> migrationId) (\s@StartMigrationResponse' {} a -> s {migrationId = a} :: StartMigrationResponse)

-- | The strategy used to conduct the migration.
startMigrationResponse_migrationStrategy :: Lens.Lens' StartMigrationResponse (Prelude.Maybe MigrationStrategy)
startMigrationResponse_migrationStrategy = Lens.lens (\StartMigrationResponse' {migrationStrategy} -> migrationStrategy) (\s@StartMigrationResponse' {} a -> s {migrationStrategy = a} :: StartMigrationResponse)

-- | The date and time that the migration started.
startMigrationResponse_migrationTimestamp :: Lens.Lens' StartMigrationResponse (Prelude.Maybe Prelude.UTCTime)
startMigrationResponse_migrationTimestamp = Lens.lens (\StartMigrationResponse' {migrationTimestamp} -> migrationTimestamp) (\s@StartMigrationResponse' {} a -> s {migrationTimestamp = a} :: StartMigrationResponse) Prelude.. Lens.mapping Data._Time

-- | The locale used for the Amazon Lex V1 bot.
startMigrationResponse_v1BotLocale :: Lens.Lens' StartMigrationResponse (Prelude.Maybe Locale)
startMigrationResponse_v1BotLocale = Lens.lens (\StartMigrationResponse' {v1BotLocale} -> v1BotLocale) (\s@StartMigrationResponse' {} a -> s {v1BotLocale = a} :: StartMigrationResponse)

-- | The name of the Amazon Lex V1 bot that you are migrating to Amazon Lex
-- V2.
startMigrationResponse_v1BotName :: Lens.Lens' StartMigrationResponse (Prelude.Maybe Prelude.Text)
startMigrationResponse_v1BotName = Lens.lens (\StartMigrationResponse' {v1BotName} -> v1BotName) (\s@StartMigrationResponse' {} a -> s {v1BotName = a} :: StartMigrationResponse)

-- | The version of the bot to migrate to Amazon Lex V2.
startMigrationResponse_v1BotVersion :: Lens.Lens' StartMigrationResponse (Prelude.Maybe Prelude.Text)
startMigrationResponse_v1BotVersion = Lens.lens (\StartMigrationResponse' {v1BotVersion} -> v1BotVersion) (\s@StartMigrationResponse' {} a -> s {v1BotVersion = a} :: StartMigrationResponse)

-- | The unique identifier for the Amazon Lex V2 bot.
startMigrationResponse_v2BotId :: Lens.Lens' StartMigrationResponse (Prelude.Maybe Prelude.Text)
startMigrationResponse_v2BotId = Lens.lens (\StartMigrationResponse' {v2BotId} -> v2BotId) (\s@StartMigrationResponse' {} a -> s {v2BotId = a} :: StartMigrationResponse)

-- | The IAM role that Amazon Lex uses to run the Amazon Lex V2 bot.
startMigrationResponse_v2BotRole :: Lens.Lens' StartMigrationResponse (Prelude.Maybe Prelude.Text)
startMigrationResponse_v2BotRole = Lens.lens (\StartMigrationResponse' {v2BotRole} -> v2BotRole) (\s@StartMigrationResponse' {} a -> s {v2BotRole = a} :: StartMigrationResponse)

-- | The response's http status code.
startMigrationResponse_httpStatus :: Lens.Lens' StartMigrationResponse Prelude.Int
startMigrationResponse_httpStatus = Lens.lens (\StartMigrationResponse' {httpStatus} -> httpStatus) (\s@StartMigrationResponse' {} a -> s {httpStatus = a} :: StartMigrationResponse)

instance Prelude.NFData StartMigrationResponse where
  rnf StartMigrationResponse' {..} =
    Prelude.rnf migrationId
      `Prelude.seq` Prelude.rnf migrationStrategy
      `Prelude.seq` Prelude.rnf migrationTimestamp
      `Prelude.seq` Prelude.rnf v1BotLocale
      `Prelude.seq` Prelude.rnf v1BotName
      `Prelude.seq` Prelude.rnf v1BotVersion
      `Prelude.seq` Prelude.rnf v2BotId
      `Prelude.seq` Prelude.rnf v2BotRole
      `Prelude.seq` Prelude.rnf httpStatus
