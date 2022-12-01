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
-- Module      : Amazonka.NetworkFirewall.Types.SourceMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.SourceMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | High-level information about the managed rule group that your own rule
-- group is copied from. You can use the the metadata to track version
-- updates made to the originating rule group. You can retrieve all objects
-- for a rule group by calling
-- <https://docs.aws.amazon.com/network-firewall/latest/APIReference/API_DescribeRuleGroup.html DescribeRuleGroup>.
--
-- /See:/ 'newSourceMetadata' smart constructor.
data SourceMetadata = SourceMetadata'
  { -- | The Amazon Resource Name (ARN) of the rule group that your own rule
    -- group is copied from.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | The update token of the Amazon Web Services managed rule group that your
    -- own rule group is copied from. To determine the update token for the
    -- managed rule group, call
    -- <https://docs.aws.amazon.com/network-firewall/latest/APIReference/API_DescribeRuleGroup.html#networkfirewall-DescribeRuleGroup-response-UpdateToken DescribeRuleGroup>.
    sourceUpdateToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceArn', 'sourceMetadata_sourceArn' - The Amazon Resource Name (ARN) of the rule group that your own rule
-- group is copied from.
--
-- 'sourceUpdateToken', 'sourceMetadata_sourceUpdateToken' - The update token of the Amazon Web Services managed rule group that your
-- own rule group is copied from. To determine the update token for the
-- managed rule group, call
-- <https://docs.aws.amazon.com/network-firewall/latest/APIReference/API_DescribeRuleGroup.html#networkfirewall-DescribeRuleGroup-response-UpdateToken DescribeRuleGroup>.
newSourceMetadata ::
  SourceMetadata
newSourceMetadata =
  SourceMetadata'
    { sourceArn = Prelude.Nothing,
      sourceUpdateToken = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the rule group that your own rule
-- group is copied from.
sourceMetadata_sourceArn :: Lens.Lens' SourceMetadata (Prelude.Maybe Prelude.Text)
sourceMetadata_sourceArn = Lens.lens (\SourceMetadata' {sourceArn} -> sourceArn) (\s@SourceMetadata' {} a -> s {sourceArn = a} :: SourceMetadata)

-- | The update token of the Amazon Web Services managed rule group that your
-- own rule group is copied from. To determine the update token for the
-- managed rule group, call
-- <https://docs.aws.amazon.com/network-firewall/latest/APIReference/API_DescribeRuleGroup.html#networkfirewall-DescribeRuleGroup-response-UpdateToken DescribeRuleGroup>.
sourceMetadata_sourceUpdateToken :: Lens.Lens' SourceMetadata (Prelude.Maybe Prelude.Text)
sourceMetadata_sourceUpdateToken = Lens.lens (\SourceMetadata' {sourceUpdateToken} -> sourceUpdateToken) (\s@SourceMetadata' {} a -> s {sourceUpdateToken = a} :: SourceMetadata)

instance Core.FromJSON SourceMetadata where
  parseJSON =
    Core.withObject
      "SourceMetadata"
      ( \x ->
          SourceMetadata'
            Prelude.<$> (x Core..:? "SourceArn")
            Prelude.<*> (x Core..:? "SourceUpdateToken")
      )

instance Prelude.Hashable SourceMetadata where
  hashWithSalt _salt SourceMetadata' {..} =
    _salt `Prelude.hashWithSalt` sourceArn
      `Prelude.hashWithSalt` sourceUpdateToken

instance Prelude.NFData SourceMetadata where
  rnf SourceMetadata' {..} =
    Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf sourceUpdateToken

instance Core.ToJSON SourceMetadata where
  toJSON SourceMetadata' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SourceArn" Core..=) Prelude.<$> sourceArn,
            ("SourceUpdateToken" Core..=)
              Prelude.<$> sourceUpdateToken
          ]
      )
