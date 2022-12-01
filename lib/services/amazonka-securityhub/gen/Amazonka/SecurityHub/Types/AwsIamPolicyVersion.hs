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
-- Module      : Amazonka.SecurityHub.Types.AwsIamPolicyVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsIamPolicyVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A version of an IAM policy.
--
-- /See:/ 'newAwsIamPolicyVersion' smart constructor.
data AwsIamPolicyVersion = AwsIamPolicyVersion'
  { -- | Whether the version is the default version.
    isDefaultVersion :: Prelude.Maybe Prelude.Bool,
    -- | Indicates when the version was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createDate :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the policy version.
    versionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsIamPolicyVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isDefaultVersion', 'awsIamPolicyVersion_isDefaultVersion' - Whether the version is the default version.
--
-- 'createDate', 'awsIamPolicyVersion_createDate' - Indicates when the version was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'versionId', 'awsIamPolicyVersion_versionId' - The identifier of the policy version.
newAwsIamPolicyVersion ::
  AwsIamPolicyVersion
newAwsIamPolicyVersion =
  AwsIamPolicyVersion'
    { isDefaultVersion =
        Prelude.Nothing,
      createDate = Prelude.Nothing,
      versionId = Prelude.Nothing
    }

-- | Whether the version is the default version.
awsIamPolicyVersion_isDefaultVersion :: Lens.Lens' AwsIamPolicyVersion (Prelude.Maybe Prelude.Bool)
awsIamPolicyVersion_isDefaultVersion = Lens.lens (\AwsIamPolicyVersion' {isDefaultVersion} -> isDefaultVersion) (\s@AwsIamPolicyVersion' {} a -> s {isDefaultVersion = a} :: AwsIamPolicyVersion)

-- | Indicates when the version was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsIamPolicyVersion_createDate :: Lens.Lens' AwsIamPolicyVersion (Prelude.Maybe Prelude.Text)
awsIamPolicyVersion_createDate = Lens.lens (\AwsIamPolicyVersion' {createDate} -> createDate) (\s@AwsIamPolicyVersion' {} a -> s {createDate = a} :: AwsIamPolicyVersion)

-- | The identifier of the policy version.
awsIamPolicyVersion_versionId :: Lens.Lens' AwsIamPolicyVersion (Prelude.Maybe Prelude.Text)
awsIamPolicyVersion_versionId = Lens.lens (\AwsIamPolicyVersion' {versionId} -> versionId) (\s@AwsIamPolicyVersion' {} a -> s {versionId = a} :: AwsIamPolicyVersion)

instance Core.FromJSON AwsIamPolicyVersion where
  parseJSON =
    Core.withObject
      "AwsIamPolicyVersion"
      ( \x ->
          AwsIamPolicyVersion'
            Prelude.<$> (x Core..:? "IsDefaultVersion")
            Prelude.<*> (x Core..:? "CreateDate")
            Prelude.<*> (x Core..:? "VersionId")
      )

instance Prelude.Hashable AwsIamPolicyVersion where
  hashWithSalt _salt AwsIamPolicyVersion' {..} =
    _salt `Prelude.hashWithSalt` isDefaultVersion
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` versionId

instance Prelude.NFData AwsIamPolicyVersion where
  rnf AwsIamPolicyVersion' {..} =
    Prelude.rnf isDefaultVersion
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf versionId

instance Core.ToJSON AwsIamPolicyVersion where
  toJSON AwsIamPolicyVersion' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("IsDefaultVersion" Core..=)
              Prelude.<$> isDefaultVersion,
            ("CreateDate" Core..=) Prelude.<$> createDate,
            ("VersionId" Core..=) Prelude.<$> versionId
          ]
      )
