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
-- Module      : Amazonka.SecurityHub.Types.AwsWafv2CustomRequestHandlingDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafv2CustomRequestHandlingDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsWafv2CustomHttpHeader

-- | Custom request handling behavior that inserts custom headers into a web
-- request. WAF uses custom request handling when the rule action doesn\'t
-- block the request.
--
-- /See:/ 'newAwsWafv2CustomRequestHandlingDetails' smart constructor.
data AwsWafv2CustomRequestHandlingDetails = AwsWafv2CustomRequestHandlingDetails'
  { -- | The HTTP headers to insert into the request.
    insertHeaders :: Prelude.Maybe [AwsWafv2CustomHttpHeader]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafv2CustomRequestHandlingDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'insertHeaders', 'awsWafv2CustomRequestHandlingDetails_insertHeaders' - The HTTP headers to insert into the request.
newAwsWafv2CustomRequestHandlingDetails ::
  AwsWafv2CustomRequestHandlingDetails
newAwsWafv2CustomRequestHandlingDetails =
  AwsWafv2CustomRequestHandlingDetails'
    { insertHeaders =
        Prelude.Nothing
    }

-- | The HTTP headers to insert into the request.
awsWafv2CustomRequestHandlingDetails_insertHeaders :: Lens.Lens' AwsWafv2CustomRequestHandlingDetails (Prelude.Maybe [AwsWafv2CustomHttpHeader])
awsWafv2CustomRequestHandlingDetails_insertHeaders = Lens.lens (\AwsWafv2CustomRequestHandlingDetails' {insertHeaders} -> insertHeaders) (\s@AwsWafv2CustomRequestHandlingDetails' {} a -> s {insertHeaders = a} :: AwsWafv2CustomRequestHandlingDetails) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    AwsWafv2CustomRequestHandlingDetails
  where
  parseJSON =
    Data.withObject
      "AwsWafv2CustomRequestHandlingDetails"
      ( \x ->
          AwsWafv2CustomRequestHandlingDetails'
            Prelude.<$> (x Data..:? "InsertHeaders" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsWafv2CustomRequestHandlingDetails
  where
  hashWithSalt
    _salt
    AwsWafv2CustomRequestHandlingDetails' {..} =
      _salt `Prelude.hashWithSalt` insertHeaders

instance
  Prelude.NFData
    AwsWafv2CustomRequestHandlingDetails
  where
  rnf AwsWafv2CustomRequestHandlingDetails' {..} =
    Prelude.rnf insertHeaders

instance
  Data.ToJSON
    AwsWafv2CustomRequestHandlingDetails
  where
  toJSON AwsWafv2CustomRequestHandlingDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InsertHeaders" Data..=)
              Prelude.<$> insertHeaders
          ]
      )
