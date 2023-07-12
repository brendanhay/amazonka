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
-- Module      : Amazonka.CloudFront.Types.FieldLevelEncryptionConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.FieldLevelEncryptionConfig where

import Amazonka.CloudFront.Types.ContentTypeProfileConfig
import Amazonka.CloudFront.Types.QueryArgProfileConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A complex data type that includes the profile configurations specified
-- for field-level encryption.
--
-- /See:/ 'newFieldLevelEncryptionConfig' smart constructor.
data FieldLevelEncryptionConfig = FieldLevelEncryptionConfig'
  { -- | An optional comment about the configuration. The comment cannot be
    -- longer than 128 characters.
    comment :: Prelude.Maybe Prelude.Text,
    -- | A complex data type that specifies when to forward content if a content
    -- type isn\'t recognized and profiles to use as by default in a request if
    -- a query argument doesn\'t specify a profile to use.
    contentTypeProfileConfig :: Prelude.Maybe ContentTypeProfileConfig,
    -- | A complex data type that specifies when to forward content if a profile
    -- isn\'t found and the profile that can be provided as a query argument in
    -- a request.
    queryArgProfileConfig :: Prelude.Maybe QueryArgProfileConfig,
    -- | A unique number that ensures the request can\'t be replayed.
    callerReference :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldLevelEncryptionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'fieldLevelEncryptionConfig_comment' - An optional comment about the configuration. The comment cannot be
-- longer than 128 characters.
--
-- 'contentTypeProfileConfig', 'fieldLevelEncryptionConfig_contentTypeProfileConfig' - A complex data type that specifies when to forward content if a content
-- type isn\'t recognized and profiles to use as by default in a request if
-- a query argument doesn\'t specify a profile to use.
--
-- 'queryArgProfileConfig', 'fieldLevelEncryptionConfig_queryArgProfileConfig' - A complex data type that specifies when to forward content if a profile
-- isn\'t found and the profile that can be provided as a query argument in
-- a request.
--
-- 'callerReference', 'fieldLevelEncryptionConfig_callerReference' - A unique number that ensures the request can\'t be replayed.
newFieldLevelEncryptionConfig ::
  -- | 'callerReference'
  Prelude.Text ->
  FieldLevelEncryptionConfig
newFieldLevelEncryptionConfig pCallerReference_ =
  FieldLevelEncryptionConfig'
    { comment =
        Prelude.Nothing,
      contentTypeProfileConfig = Prelude.Nothing,
      queryArgProfileConfig = Prelude.Nothing,
      callerReference = pCallerReference_
    }

-- | An optional comment about the configuration. The comment cannot be
-- longer than 128 characters.
fieldLevelEncryptionConfig_comment :: Lens.Lens' FieldLevelEncryptionConfig (Prelude.Maybe Prelude.Text)
fieldLevelEncryptionConfig_comment = Lens.lens (\FieldLevelEncryptionConfig' {comment} -> comment) (\s@FieldLevelEncryptionConfig' {} a -> s {comment = a} :: FieldLevelEncryptionConfig)

-- | A complex data type that specifies when to forward content if a content
-- type isn\'t recognized and profiles to use as by default in a request if
-- a query argument doesn\'t specify a profile to use.
fieldLevelEncryptionConfig_contentTypeProfileConfig :: Lens.Lens' FieldLevelEncryptionConfig (Prelude.Maybe ContentTypeProfileConfig)
fieldLevelEncryptionConfig_contentTypeProfileConfig = Lens.lens (\FieldLevelEncryptionConfig' {contentTypeProfileConfig} -> contentTypeProfileConfig) (\s@FieldLevelEncryptionConfig' {} a -> s {contentTypeProfileConfig = a} :: FieldLevelEncryptionConfig)

-- | A complex data type that specifies when to forward content if a profile
-- isn\'t found and the profile that can be provided as a query argument in
-- a request.
fieldLevelEncryptionConfig_queryArgProfileConfig :: Lens.Lens' FieldLevelEncryptionConfig (Prelude.Maybe QueryArgProfileConfig)
fieldLevelEncryptionConfig_queryArgProfileConfig = Lens.lens (\FieldLevelEncryptionConfig' {queryArgProfileConfig} -> queryArgProfileConfig) (\s@FieldLevelEncryptionConfig' {} a -> s {queryArgProfileConfig = a} :: FieldLevelEncryptionConfig)

-- | A unique number that ensures the request can\'t be replayed.
fieldLevelEncryptionConfig_callerReference :: Lens.Lens' FieldLevelEncryptionConfig Prelude.Text
fieldLevelEncryptionConfig_callerReference = Lens.lens (\FieldLevelEncryptionConfig' {callerReference} -> callerReference) (\s@FieldLevelEncryptionConfig' {} a -> s {callerReference = a} :: FieldLevelEncryptionConfig)

instance Data.FromXML FieldLevelEncryptionConfig where
  parseXML x =
    FieldLevelEncryptionConfig'
      Prelude.<$> (x Data..@? "Comment")
      Prelude.<*> (x Data..@? "ContentTypeProfileConfig")
      Prelude.<*> (x Data..@? "QueryArgProfileConfig")
      Prelude.<*> (x Data..@ "CallerReference")

instance Prelude.Hashable FieldLevelEncryptionConfig where
  hashWithSalt _salt FieldLevelEncryptionConfig' {..} =
    _salt
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` contentTypeProfileConfig
      `Prelude.hashWithSalt` queryArgProfileConfig
      `Prelude.hashWithSalt` callerReference

instance Prelude.NFData FieldLevelEncryptionConfig where
  rnf FieldLevelEncryptionConfig' {..} =
    Prelude.rnf comment
      `Prelude.seq` Prelude.rnf contentTypeProfileConfig
      `Prelude.seq` Prelude.rnf queryArgProfileConfig
      `Prelude.seq` Prelude.rnf callerReference

instance Data.ToXML FieldLevelEncryptionConfig where
  toXML FieldLevelEncryptionConfig' {..} =
    Prelude.mconcat
      [ "Comment" Data.@= comment,
        "ContentTypeProfileConfig"
          Data.@= contentTypeProfileConfig,
        "QueryArgProfileConfig"
          Data.@= queryArgProfileConfig,
        "CallerReference" Data.@= callerReference
      ]
