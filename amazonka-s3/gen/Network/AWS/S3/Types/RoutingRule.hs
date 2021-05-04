{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.S3.Types.RoutingRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.RoutingRule where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Condition
import Network.AWS.S3.Types.Redirect

-- | Specifies the redirect behavior and when a redirect is applied. For more
-- information about routing rules, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/how-to-page-redirect.html#advanced-conditional-redirects Configuring advanced conditional redirects>
-- in the /Amazon Simple Storage Service Developer Guide/.
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromXML RoutingRule where
  parseXML x =
    RoutingRule'
      Prelude.<$> (x Prelude..@? "Condition")
      Prelude.<*> (x Prelude..@ "Redirect")

instance Prelude.Hashable RoutingRule

instance Prelude.NFData RoutingRule

instance Prelude.ToXML RoutingRule where
  toXML RoutingRule' {..} =
    Prelude.mconcat
      [ "Condition" Prelude.@= condition,
        "Redirect" Prelude.@= redirect
      ]
