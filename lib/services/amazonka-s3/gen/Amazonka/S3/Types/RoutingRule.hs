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
-- Module      : Amazonka.S3.Types.RoutingRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.RoutingRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.Condition
import Amazonka.S3.Types.Redirect

-- | Specifies the redirect behavior and when a redirect is applied. For more
-- information about routing rules, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/how-to-page-redirect.html#advanced-conditional-redirects Configuring advanced conditional redirects>
-- in the /Amazon S3 User Guide/.
--
-- /See:/ 'newRoutingRule' smart constructor.
data RoutingRule = RoutingRule'
  { -- | A container for describing a condition that must be met for the
    -- specified redirect to apply. For example, 1. If request is for pages in
    -- the @\/docs@ folder, redirect to the @\/documents@ folder. 2. If request
    -- results in HTTP error 4xx, redirect request to another host where you
    -- might process the error.
    condition :: Prelude.Maybe Condition,
    -- | Container for redirect information. You can redirect requests to another
    -- host, to another page, or with another protocol. In the event of an
    -- error, you can specify a different error code to return.
    redirect :: Redirect
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RoutingRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'condition', 'routingRule_condition' - A container for describing a condition that must be met for the
-- specified redirect to apply. For example, 1. If request is for pages in
-- the @\/docs@ folder, redirect to the @\/documents@ folder. 2. If request
-- results in HTTP error 4xx, redirect request to another host where you
-- might process the error.
--
-- 'redirect', 'routingRule_redirect' - Container for redirect information. You can redirect requests to another
-- host, to another page, or with another protocol. In the event of an
-- error, you can specify a different error code to return.
newRoutingRule ::
  -- | 'redirect'
  Redirect ->
  RoutingRule
newRoutingRule pRedirect_ =
  RoutingRule'
    { condition = Prelude.Nothing,
      redirect = pRedirect_
    }

-- | A container for describing a condition that must be met for the
-- specified redirect to apply. For example, 1. If request is for pages in
-- the @\/docs@ folder, redirect to the @\/documents@ folder. 2. If request
-- results in HTTP error 4xx, redirect request to another host where you
-- might process the error.
routingRule_condition :: Lens.Lens' RoutingRule (Prelude.Maybe Condition)
routingRule_condition = Lens.lens (\RoutingRule' {condition} -> condition) (\s@RoutingRule' {} a -> s {condition = a} :: RoutingRule)

-- | Container for redirect information. You can redirect requests to another
-- host, to another page, or with another protocol. In the event of an
-- error, you can specify a different error code to return.
routingRule_redirect :: Lens.Lens' RoutingRule Redirect
routingRule_redirect = Lens.lens (\RoutingRule' {redirect} -> redirect) (\s@RoutingRule' {} a -> s {redirect = a} :: RoutingRule)

instance Data.FromXML RoutingRule where
  parseXML x =
    RoutingRule'
      Prelude.<$> (x Data..@? "Condition")
      Prelude.<*> (x Data..@ "Redirect")

instance Prelude.Hashable RoutingRule where
  hashWithSalt _salt RoutingRule' {..} =
    _salt
      `Prelude.hashWithSalt` condition
      `Prelude.hashWithSalt` redirect

instance Prelude.NFData RoutingRule where
  rnf RoutingRule' {..} =
    Prelude.rnf condition
      `Prelude.seq` Prelude.rnf redirect

instance Data.ToXML RoutingRule where
  toXML RoutingRule' {..} =
    Prelude.mconcat
      [ "Condition" Data.@= condition,
        "Redirect" Data.@= redirect
      ]
