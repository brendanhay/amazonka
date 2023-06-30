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
-- Module      : Amazonka.S3.Types.WebsiteConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.WebsiteConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.ErrorDocument
import Amazonka.S3.Types.IndexDocument
import Amazonka.S3.Types.RedirectAllRequestsTo
import Amazonka.S3.Types.RoutingRule

-- | Specifies website configuration parameters for an Amazon S3 bucket.
--
-- /See:/ 'newWebsiteConfiguration' smart constructor.
data WebsiteConfiguration = WebsiteConfiguration'
  { -- | The name of the error document for the website.
    errorDocument :: Prelude.Maybe ErrorDocument,
    -- | The name of the index document for the website.
    indexDocument :: Prelude.Maybe IndexDocument,
    -- | The redirect behavior for every request to this bucket\'s website
    -- endpoint.
    --
    -- If you specify this property, you can\'t specify any other property.
    redirectAllRequestsTo :: Prelude.Maybe RedirectAllRequestsTo,
    -- | Rules that define when a redirect is applied and the redirect behavior.
    routingRules :: Prelude.Maybe [RoutingRule]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WebsiteConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorDocument', 'websiteConfiguration_errorDocument' - The name of the error document for the website.
--
-- 'indexDocument', 'websiteConfiguration_indexDocument' - The name of the index document for the website.
--
-- 'redirectAllRequestsTo', 'websiteConfiguration_redirectAllRequestsTo' - The redirect behavior for every request to this bucket\'s website
-- endpoint.
--
-- If you specify this property, you can\'t specify any other property.
--
-- 'routingRules', 'websiteConfiguration_routingRules' - Rules that define when a redirect is applied and the redirect behavior.
newWebsiteConfiguration ::
  WebsiteConfiguration
newWebsiteConfiguration =
  WebsiteConfiguration'
    { errorDocument =
        Prelude.Nothing,
      indexDocument = Prelude.Nothing,
      redirectAllRequestsTo = Prelude.Nothing,
      routingRules = Prelude.Nothing
    }

-- | The name of the error document for the website.
websiteConfiguration_errorDocument :: Lens.Lens' WebsiteConfiguration (Prelude.Maybe ErrorDocument)
websiteConfiguration_errorDocument = Lens.lens (\WebsiteConfiguration' {errorDocument} -> errorDocument) (\s@WebsiteConfiguration' {} a -> s {errorDocument = a} :: WebsiteConfiguration)

-- | The name of the index document for the website.
websiteConfiguration_indexDocument :: Lens.Lens' WebsiteConfiguration (Prelude.Maybe IndexDocument)
websiteConfiguration_indexDocument = Lens.lens (\WebsiteConfiguration' {indexDocument} -> indexDocument) (\s@WebsiteConfiguration' {} a -> s {indexDocument = a} :: WebsiteConfiguration)

-- | The redirect behavior for every request to this bucket\'s website
-- endpoint.
--
-- If you specify this property, you can\'t specify any other property.
websiteConfiguration_redirectAllRequestsTo :: Lens.Lens' WebsiteConfiguration (Prelude.Maybe RedirectAllRequestsTo)
websiteConfiguration_redirectAllRequestsTo = Lens.lens (\WebsiteConfiguration' {redirectAllRequestsTo} -> redirectAllRequestsTo) (\s@WebsiteConfiguration' {} a -> s {redirectAllRequestsTo = a} :: WebsiteConfiguration)

-- | Rules that define when a redirect is applied and the redirect behavior.
websiteConfiguration_routingRules :: Lens.Lens' WebsiteConfiguration (Prelude.Maybe [RoutingRule])
websiteConfiguration_routingRules = Lens.lens (\WebsiteConfiguration' {routingRules} -> routingRules) (\s@WebsiteConfiguration' {} a -> s {routingRules = a} :: WebsiteConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable WebsiteConfiguration where
  hashWithSalt _salt WebsiteConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` errorDocument
      `Prelude.hashWithSalt` indexDocument
      `Prelude.hashWithSalt` redirectAllRequestsTo
      `Prelude.hashWithSalt` routingRules

instance Prelude.NFData WebsiteConfiguration where
  rnf WebsiteConfiguration' {..} =
    Prelude.rnf errorDocument
      `Prelude.seq` Prelude.rnf indexDocument
      `Prelude.seq` Prelude.rnf redirectAllRequestsTo
      `Prelude.seq` Prelude.rnf routingRules

instance Data.ToXML WebsiteConfiguration where
  toXML WebsiteConfiguration' {..} =
    Prelude.mconcat
      [ "ErrorDocument" Data.@= errorDocument,
        "IndexDocument" Data.@= indexDocument,
        "RedirectAllRequestsTo"
          Data.@= redirectAllRequestsTo,
        "RoutingRules"
          Data.@= Data.toXML
            ( Data.toXMLList "RoutingRule"
                Prelude.<$> routingRules
            )
      ]
