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
-- Module      : Amazonka.ELBV2.Types.SourceIpConditionConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.SourceIpConditionConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a source IP condition.
--
-- You can use this condition to route based on the IP address of the
-- source that connects to the load balancer. If a client is behind a
-- proxy, this is the IP address of the proxy not the IP address of the
-- client.
--
-- /See:/ 'newSourceIpConditionConfig' smart constructor.
data SourceIpConditionConfig = SourceIpConditionConfig'
  { -- | The source IP addresses, in CIDR format. You can use both IPv4 and IPv6
    -- addresses. Wildcards are not supported.
    --
    -- If you specify multiple addresses, the condition is satisfied if the
    -- source IP address of the request matches one of the CIDR blocks. This
    -- condition is not satisfied by the addresses in the X-Forwarded-For
    -- header. To search for addresses in the X-Forwarded-For header, use
    -- HttpHeaderConditionConfig.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceIpConditionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'sourceIpConditionConfig_values' - The source IP addresses, in CIDR format. You can use both IPv4 and IPv6
-- addresses. Wildcards are not supported.
--
-- If you specify multiple addresses, the condition is satisfied if the
-- source IP address of the request matches one of the CIDR blocks. This
-- condition is not satisfied by the addresses in the X-Forwarded-For
-- header. To search for addresses in the X-Forwarded-For header, use
-- HttpHeaderConditionConfig.
newSourceIpConditionConfig ::
  SourceIpConditionConfig
newSourceIpConditionConfig =
  SourceIpConditionConfig' {values = Prelude.Nothing}

-- | The source IP addresses, in CIDR format. You can use both IPv4 and IPv6
-- addresses. Wildcards are not supported.
--
-- If you specify multiple addresses, the condition is satisfied if the
-- source IP address of the request matches one of the CIDR blocks. This
-- condition is not satisfied by the addresses in the X-Forwarded-For
-- header. To search for addresses in the X-Forwarded-For header, use
-- HttpHeaderConditionConfig.
sourceIpConditionConfig_values :: Lens.Lens' SourceIpConditionConfig (Prelude.Maybe [Prelude.Text])
sourceIpConditionConfig_values = Lens.lens (\SourceIpConditionConfig' {values} -> values) (\s@SourceIpConditionConfig' {} a -> s {values = a} :: SourceIpConditionConfig) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML SourceIpConditionConfig where
  parseXML x =
    SourceIpConditionConfig'
      Prelude.<$> ( x Data..@? "Values" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )

instance Prelude.Hashable SourceIpConditionConfig where
  hashWithSalt _salt SourceIpConditionConfig' {..} =
    _salt `Prelude.hashWithSalt` values

instance Prelude.NFData SourceIpConditionConfig where
  rnf SourceIpConditionConfig' {..} = Prelude.rnf values

instance Data.ToQuery SourceIpConditionConfig where
  toQuery SourceIpConditionConfig' {..} =
    Prelude.mconcat
      [ "Values"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> values)
      ]
