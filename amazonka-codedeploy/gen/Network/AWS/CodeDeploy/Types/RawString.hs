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
-- Module      : Network.AWS.CodeDeploy.Types.RawString
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.RawString where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A revision for an AWS Lambda deployment that is a YAML-formatted or
-- JSON-formatted string. For AWS Lambda deployments, the revision is the
-- same as the AppSpec file.
--
-- /See:/ 'newRawString' smart constructor.
data RawString = RawString'
  { -- | The YAML-formatted or JSON-formatted revision string. It includes
    -- information about which Lambda function to update and optional Lambda
    -- functions that validate deployment lifecycle events.
    content :: Core.Maybe Core.Text,
    -- | The SHA256 hash value of the revision content.
    sha256 :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RawString' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'content', 'rawString_content' - The YAML-formatted or JSON-formatted revision string. It includes
-- information about which Lambda function to update and optional Lambda
-- functions that validate deployment lifecycle events.
--
-- 'sha256', 'rawString_sha256' - The SHA256 hash value of the revision content.
newRawString ::
  RawString
newRawString =
  RawString'
    { content = Core.Nothing,
      sha256 = Core.Nothing
    }

-- | The YAML-formatted or JSON-formatted revision string. It includes
-- information about which Lambda function to update and optional Lambda
-- functions that validate deployment lifecycle events.
rawString_content :: Lens.Lens' RawString (Core.Maybe Core.Text)
rawString_content = Lens.lens (\RawString' {content} -> content) (\s@RawString' {} a -> s {content = a} :: RawString)

-- | The SHA256 hash value of the revision content.
rawString_sha256 :: Lens.Lens' RawString (Core.Maybe Core.Text)
rawString_sha256 = Lens.lens (\RawString' {sha256} -> sha256) (\s@RawString' {} a -> s {sha256 = a} :: RawString)

instance Core.FromJSON RawString where
  parseJSON =
    Core.withObject
      "RawString"
      ( \x ->
          RawString'
            Core.<$> (x Core..:? "content")
            Core.<*> (x Core..:? "sha256")
      )

instance Core.Hashable RawString

instance Core.NFData RawString

instance Core.ToJSON RawString where
  toJSON RawString' {..} =
    Core.object
      ( Core.catMaybes
          [ ("content" Core..=) Core.<$> content,
            ("sha256" Core..=) Core.<$> sha256
          ]
      )
