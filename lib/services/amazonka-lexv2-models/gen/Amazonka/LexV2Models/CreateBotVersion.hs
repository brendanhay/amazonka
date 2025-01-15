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
-- Module      : Amazonka.LexV2Models.CreateBotVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of the bot based on the @DRAFT@ version. If the
-- @DRAFT@ version of this resource hasn\'t changed since you created the
-- last version, Amazon Lex doesn\'t create a new version, it returns the
-- last created version.
--
-- When you create the first version of a bot, Amazon Lex sets the version
-- to 1. Subsequent versions increment by 1.
module Amazonka.LexV2Models.CreateBotVersion
  ( -- * Creating a Request
    CreateBotVersion (..),
    newCreateBotVersion,

    -- * Request Lenses
    createBotVersion_description,
    createBotVersion_botId,
    createBotVersion_botVersionLocaleSpecification,

    -- * Destructuring the Response
    CreateBotVersionResponse (..),
    newCreateBotVersionResponse,

    -- * Response Lenses
    createBotVersionResponse_botId,
    createBotVersionResponse_botStatus,
    createBotVersionResponse_botVersion,
    createBotVersionResponse_botVersionLocaleSpecification,
    createBotVersionResponse_creationDateTime,
    createBotVersionResponse_description,
    createBotVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateBotVersion' smart constructor.
data CreateBotVersion = CreateBotVersion'
  { -- | A description of the version. Use the description to help identify the
    -- version in lists.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the bot to create the version for.
    botId :: Prelude.Text,
    -- | Specifies the locales that Amazon Lex adds to this version. You can
    -- choose the @Draft@ version or any other previously published version for
    -- each locale. When you specify a source version, the locale data is
    -- copied from the source version to the new version.
    botVersionLocaleSpecification :: Prelude.HashMap Prelude.Text BotVersionLocaleDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBotVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createBotVersion_description' - A description of the version. Use the description to help identify the
-- version in lists.
--
-- 'botId', 'createBotVersion_botId' - The identifier of the bot to create the version for.
--
-- 'botVersionLocaleSpecification', 'createBotVersion_botVersionLocaleSpecification' - Specifies the locales that Amazon Lex adds to this version. You can
-- choose the @Draft@ version or any other previously published version for
-- each locale. When you specify a source version, the locale data is
-- copied from the source version to the new version.
newCreateBotVersion ::
  -- | 'botId'
  Prelude.Text ->
  CreateBotVersion
newCreateBotVersion pBotId_ =
  CreateBotVersion'
    { description = Prelude.Nothing,
      botId = pBotId_,
      botVersionLocaleSpecification = Prelude.mempty
    }

-- | A description of the version. Use the description to help identify the
-- version in lists.
createBotVersion_description :: Lens.Lens' CreateBotVersion (Prelude.Maybe Prelude.Text)
createBotVersion_description = Lens.lens (\CreateBotVersion' {description} -> description) (\s@CreateBotVersion' {} a -> s {description = a} :: CreateBotVersion)

-- | The identifier of the bot to create the version for.
createBotVersion_botId :: Lens.Lens' CreateBotVersion Prelude.Text
createBotVersion_botId = Lens.lens (\CreateBotVersion' {botId} -> botId) (\s@CreateBotVersion' {} a -> s {botId = a} :: CreateBotVersion)

-- | Specifies the locales that Amazon Lex adds to this version. You can
-- choose the @Draft@ version or any other previously published version for
-- each locale. When you specify a source version, the locale data is
-- copied from the source version to the new version.
createBotVersion_botVersionLocaleSpecification :: Lens.Lens' CreateBotVersion (Prelude.HashMap Prelude.Text BotVersionLocaleDetails)
createBotVersion_botVersionLocaleSpecification = Lens.lens (\CreateBotVersion' {botVersionLocaleSpecification} -> botVersionLocaleSpecification) (\s@CreateBotVersion' {} a -> s {botVersionLocaleSpecification = a} :: CreateBotVersion) Prelude.. Lens.coerced

instance Core.AWSRequest CreateBotVersion where
  type
    AWSResponse CreateBotVersion =
      CreateBotVersionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBotVersionResponse'
            Prelude.<$> (x Data..?> "botId")
            Prelude.<*> (x Data..?> "botStatus")
            Prelude.<*> (x Data..?> "botVersion")
            Prelude.<*> ( x
                            Data..?> "botVersionLocaleSpecification"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "creationDateTime")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBotVersion where
  hashWithSalt _salt CreateBotVersion' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersionLocaleSpecification

instance Prelude.NFData CreateBotVersion where
  rnf CreateBotVersion' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf botId `Prelude.seq`
        Prelude.rnf botVersionLocaleSpecification

instance Data.ToHeaders CreateBotVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateBotVersion where
  toJSON CreateBotVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            Prelude.Just
              ( "botVersionLocaleSpecification"
                  Data..= botVersionLocaleSpecification
              )
          ]
      )

instance Data.ToPath CreateBotVersion where
  toPath CreateBotVersion' {..} =
    Prelude.mconcat
      ["/bots/", Data.toBS botId, "/botversions/"]

instance Data.ToQuery CreateBotVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBotVersionResponse' smart constructor.
data CreateBotVersionResponse = CreateBotVersionResponse'
  { -- | The bot identifier specified in the request.
    botId :: Prelude.Maybe Prelude.Text,
    -- | When you send a request to create or update a bot, Amazon Lex sets the
    -- status response element to @Creating@. After Amazon Lex builds the bot,
    -- it sets status to @Available@. If Amazon Lex can\'t build the bot, it
    -- sets status to @Failed@.
    botStatus :: Prelude.Maybe BotStatus,
    -- | The version number assigned to the version.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The source versions used for each locale in the new version.
    botVersionLocaleSpecification :: Prelude.Maybe (Prelude.HashMap Prelude.Text BotVersionLocaleDetails),
    -- | A timestamp of the date and time that the version was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the version specified in the request.
    description :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBotVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'createBotVersionResponse_botId' - The bot identifier specified in the request.
--
-- 'botStatus', 'createBotVersionResponse_botStatus' - When you send a request to create or update a bot, Amazon Lex sets the
-- status response element to @Creating@. After Amazon Lex builds the bot,
-- it sets status to @Available@. If Amazon Lex can\'t build the bot, it
-- sets status to @Failed@.
--
-- 'botVersion', 'createBotVersionResponse_botVersion' - The version number assigned to the version.
--
-- 'botVersionLocaleSpecification', 'createBotVersionResponse_botVersionLocaleSpecification' - The source versions used for each locale in the new version.
--
-- 'creationDateTime', 'createBotVersionResponse_creationDateTime' - A timestamp of the date and time that the version was created.
--
-- 'description', 'createBotVersionResponse_description' - The description of the version specified in the request.
--
-- 'httpStatus', 'createBotVersionResponse_httpStatus' - The response's http status code.
newCreateBotVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBotVersionResponse
newCreateBotVersionResponse pHttpStatus_ =
  CreateBotVersionResponse'
    { botId = Prelude.Nothing,
      botStatus = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      botVersionLocaleSpecification = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      description = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The bot identifier specified in the request.
createBotVersionResponse_botId :: Lens.Lens' CreateBotVersionResponse (Prelude.Maybe Prelude.Text)
createBotVersionResponse_botId = Lens.lens (\CreateBotVersionResponse' {botId} -> botId) (\s@CreateBotVersionResponse' {} a -> s {botId = a} :: CreateBotVersionResponse)

-- | When you send a request to create or update a bot, Amazon Lex sets the
-- status response element to @Creating@. After Amazon Lex builds the bot,
-- it sets status to @Available@. If Amazon Lex can\'t build the bot, it
-- sets status to @Failed@.
createBotVersionResponse_botStatus :: Lens.Lens' CreateBotVersionResponse (Prelude.Maybe BotStatus)
createBotVersionResponse_botStatus = Lens.lens (\CreateBotVersionResponse' {botStatus} -> botStatus) (\s@CreateBotVersionResponse' {} a -> s {botStatus = a} :: CreateBotVersionResponse)

-- | The version number assigned to the version.
createBotVersionResponse_botVersion :: Lens.Lens' CreateBotVersionResponse (Prelude.Maybe Prelude.Text)
createBotVersionResponse_botVersion = Lens.lens (\CreateBotVersionResponse' {botVersion} -> botVersion) (\s@CreateBotVersionResponse' {} a -> s {botVersion = a} :: CreateBotVersionResponse)

-- | The source versions used for each locale in the new version.
createBotVersionResponse_botVersionLocaleSpecification :: Lens.Lens' CreateBotVersionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text BotVersionLocaleDetails))
createBotVersionResponse_botVersionLocaleSpecification = Lens.lens (\CreateBotVersionResponse' {botVersionLocaleSpecification} -> botVersionLocaleSpecification) (\s@CreateBotVersionResponse' {} a -> s {botVersionLocaleSpecification = a} :: CreateBotVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | A timestamp of the date and time that the version was created.
createBotVersionResponse_creationDateTime :: Lens.Lens' CreateBotVersionResponse (Prelude.Maybe Prelude.UTCTime)
createBotVersionResponse_creationDateTime = Lens.lens (\CreateBotVersionResponse' {creationDateTime} -> creationDateTime) (\s@CreateBotVersionResponse' {} a -> s {creationDateTime = a} :: CreateBotVersionResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the version specified in the request.
createBotVersionResponse_description :: Lens.Lens' CreateBotVersionResponse (Prelude.Maybe Prelude.Text)
createBotVersionResponse_description = Lens.lens (\CreateBotVersionResponse' {description} -> description) (\s@CreateBotVersionResponse' {} a -> s {description = a} :: CreateBotVersionResponse)

-- | The response's http status code.
createBotVersionResponse_httpStatus :: Lens.Lens' CreateBotVersionResponse Prelude.Int
createBotVersionResponse_httpStatus = Lens.lens (\CreateBotVersionResponse' {httpStatus} -> httpStatus) (\s@CreateBotVersionResponse' {} a -> s {httpStatus = a} :: CreateBotVersionResponse)

instance Prelude.NFData CreateBotVersionResponse where
  rnf CreateBotVersionResponse' {..} =
    Prelude.rnf botId `Prelude.seq`
      Prelude.rnf botStatus `Prelude.seq`
        Prelude.rnf botVersion `Prelude.seq`
          Prelude.rnf botVersionLocaleSpecification `Prelude.seq`
            Prelude.rnf creationDateTime `Prelude.seq`
              Prelude.rnf description `Prelude.seq`
                Prelude.rnf httpStatus
