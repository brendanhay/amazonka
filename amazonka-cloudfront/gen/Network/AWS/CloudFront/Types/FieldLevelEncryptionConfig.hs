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
-- Module      : Network.AWS.CloudFront.Types.FieldLevelEncryptionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.FieldLevelEncryptionConfig where

import Network.AWS.CloudFront.Types.ContentTypeProfileConfig
import Network.AWS.CloudFront.Types.QueryArgProfileConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A complex data type that includes the profile configurations specified
-- for field-level encryption.
--
-- /See:/ 'newFieldLevelEncryptionConfig' smart constructor.
data FieldLevelEncryptionConfig = FieldLevelEncryptionConfig'
  { -- | An optional comment about the configuration.
    comment :: Core.Maybe Core.Text,
    -- | A complex data type that specifies when to forward content if a content
    -- type isn\'t recognized and profiles to use as by default in a request if
    -- a query argument doesn\'t specify a profile to use.
    contentTypeProfileConfig :: Core.Maybe ContentTypeProfileConfig,
    -- | A complex data type that specifies when to forward content if a profile
    -- isn\'t found and the profile that can be provided as a query argument in
    -- a request.
    queryArgProfileConfig :: Core.Maybe QueryArgProfileConfig,
    -- | A unique number that ensures the request can\'t be replayed.
    callerReference :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FieldLevelEncryptionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'fieldLevelEncryptionConfig_comment' - An optional comment about the configuration.
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
  Core.Text ->
  FieldLevelEncryptionConfig
newFieldLevelEncryptionConfig pCallerReference_ =
  FieldLevelEncryptionConfig'
    { comment = Core.Nothing,
      contentTypeProfileConfig = Core.Nothing,
      queryArgProfileConfig = Core.Nothing,
      callerReference = pCallerReference_
    }

-- | An optional comment about the configuration.
fieldLevelEncryptionConfig_comment :: Lens.Lens' FieldLevelEncryptionConfig (Core.Maybe Core.Text)
fieldLevelEncryptionConfig_comment = Lens.lens (\FieldLevelEncryptionConfig' {comment} -> comment) (\s@FieldLevelEncryptionConfig' {} a -> s {comment = a} :: FieldLevelEncryptionConfig)

-- | A complex data type that specifies when to forward content if a content
-- type isn\'t recognized and profiles to use as by default in a request if
-- a query argument doesn\'t specify a profile to use.
fieldLevelEncryptionConfig_contentTypeProfileConfig :: Lens.Lens' FieldLevelEncryptionConfig (Core.Maybe ContentTypeProfileConfig)
fieldLevelEncryptionConfig_contentTypeProfileConfig = Lens.lens (\FieldLevelEncryptionConfig' {contentTypeProfileConfig} -> contentTypeProfileConfig) (\s@FieldLevelEncryptionConfig' {} a -> s {contentTypeProfileConfig = a} :: FieldLevelEncryptionConfig)

-- | A complex data type that specifies when to forward content if a profile
-- isn\'t found and the profile that can be provided as a query argument in
-- a request.
fieldLevelEncryptionConfig_queryArgProfileConfig :: Lens.Lens' FieldLevelEncryptionConfig (Core.Maybe QueryArgProfileConfig)
fieldLevelEncryptionConfig_queryArgProfileConfig = Lens.lens (\FieldLevelEncryptionConfig' {queryArgProfileConfig} -> queryArgProfileConfig) (\s@FieldLevelEncryptionConfig' {} a -> s {queryArgProfileConfig = a} :: FieldLevelEncryptionConfig)

-- | A unique number that ensures the request can\'t be replayed.
fieldLevelEncryptionConfig_callerReference :: Lens.Lens' FieldLevelEncryptionConfig Core.Text
fieldLevelEncryptionConfig_callerReference = Lens.lens (\FieldLevelEncryptionConfig' {callerReference} -> callerReference) (\s@FieldLevelEncryptionConfig' {} a -> s {callerReference = a} :: FieldLevelEncryptionConfig)

instance Core.FromXML FieldLevelEncryptionConfig where
  parseXML x =
    FieldLevelEncryptionConfig'
      Core.<$> (x Core..@? "Comment")
      Core.<*> (x Core..@? "ContentTypeProfileConfig")
      Core.<*> (x Core..@? "QueryArgProfileConfig")
      Core.<*> (x Core..@ "CallerReference")

instance Core.Hashable FieldLevelEncryptionConfig

instance Core.NFData FieldLevelEncryptionConfig

instance Core.ToXML FieldLevelEncryptionConfig where
  toXML FieldLevelEncryptionConfig' {..} =
    Core.mconcat
      [ "Comment" Core.@= comment,
        "ContentTypeProfileConfig"
          Core.@= contentTypeProfileConfig,
        "QueryArgProfileConfig"
          Core.@= queryArgProfileConfig,
        "CallerReference" Core.@= callerReference
      ]
