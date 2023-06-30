{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Route53.CreateCidrCollection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a CIDR collection in the current Amazon Web Services account.
module Amazonka.Route53.CreateCidrCollection
  ( -- * Creating a Request
    CreateCidrCollection (..),
    newCreateCidrCollection,

    -- * Request Lenses
    createCidrCollection_name,
    createCidrCollection_callerReference,

    -- * Destructuring the Response
    CreateCidrCollectionResponse (..),
    newCreateCidrCollectionResponse,

    -- * Response Lenses
    createCidrCollectionResponse_collection,
    createCidrCollectionResponse_location,
    createCidrCollectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | /See:/ 'newCreateCidrCollection' smart constructor.
data CreateCidrCollection = CreateCidrCollection'
  { -- | A unique identifier for the account that can be used to reference the
    -- collection from other API calls.
    name :: Prelude.Text,
    -- | A client-specific token that allows requests to be securely retried so
    -- that the intended outcome will only occur once, retries receive a
    -- similar response, and there are no additional edge cases to handle.
    callerReference :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCidrCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createCidrCollection_name' - A unique identifier for the account that can be used to reference the
-- collection from other API calls.
--
-- 'callerReference', 'createCidrCollection_callerReference' - A client-specific token that allows requests to be securely retried so
-- that the intended outcome will only occur once, retries receive a
-- similar response, and there are no additional edge cases to handle.
newCreateCidrCollection ::
  -- | 'name'
  Prelude.Text ->
  -- | 'callerReference'
  Prelude.Text ->
  CreateCidrCollection
newCreateCidrCollection pName_ pCallerReference_ =
  CreateCidrCollection'
    { name = pName_,
      callerReference = pCallerReference_
    }

-- | A unique identifier for the account that can be used to reference the
-- collection from other API calls.
createCidrCollection_name :: Lens.Lens' CreateCidrCollection Prelude.Text
createCidrCollection_name = Lens.lens (\CreateCidrCollection' {name} -> name) (\s@CreateCidrCollection' {} a -> s {name = a} :: CreateCidrCollection)

-- | A client-specific token that allows requests to be securely retried so
-- that the intended outcome will only occur once, retries receive a
-- similar response, and there are no additional edge cases to handle.
createCidrCollection_callerReference :: Lens.Lens' CreateCidrCollection Prelude.Text
createCidrCollection_callerReference = Lens.lens (\CreateCidrCollection' {callerReference} -> callerReference) (\s@CreateCidrCollection' {} a -> s {callerReference = a} :: CreateCidrCollection)

instance Core.AWSRequest CreateCidrCollection where
  type
    AWSResponse CreateCidrCollection =
      CreateCidrCollectionResponse
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateCidrCollectionResponse'
            Prelude.<$> (x Data..@? "Collection")
            Prelude.<*> (h Data..#? "Location")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCidrCollection where
  hashWithSalt _salt CreateCidrCollection' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` callerReference

instance Prelude.NFData CreateCidrCollection where
  rnf CreateCidrCollection' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf callerReference

instance Data.ToElement CreateCidrCollection where
  toElement =
    Data.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}CreateCidrCollectionRequest"

instance Data.ToHeaders CreateCidrCollection where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateCidrCollection where
  toPath = Prelude.const "/2013-04-01/cidrcollection"

instance Data.ToQuery CreateCidrCollection where
  toQuery = Prelude.const Prelude.mempty

instance Data.ToXML CreateCidrCollection where
  toXML CreateCidrCollection' {..} =
    Prelude.mconcat
      [ "Name" Data.@= name,
        "CallerReference" Data.@= callerReference
      ]

-- | /See:/ 'newCreateCidrCollectionResponse' smart constructor.
data CreateCidrCollectionResponse = CreateCidrCollectionResponse'
  { -- | A complex type that contains information about the CIDR collection.
    collection :: Prelude.Maybe CidrCollection,
    -- | A unique URL that represents the location for the CIDR collection.
    location :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCidrCollectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collection', 'createCidrCollectionResponse_collection' - A complex type that contains information about the CIDR collection.
--
-- 'location', 'createCidrCollectionResponse_location' - A unique URL that represents the location for the CIDR collection.
--
-- 'httpStatus', 'createCidrCollectionResponse_httpStatus' - The response's http status code.
newCreateCidrCollectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCidrCollectionResponse
newCreateCidrCollectionResponse pHttpStatus_ =
  CreateCidrCollectionResponse'
    { collection =
        Prelude.Nothing,
      location = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A complex type that contains information about the CIDR collection.
createCidrCollectionResponse_collection :: Lens.Lens' CreateCidrCollectionResponse (Prelude.Maybe CidrCollection)
createCidrCollectionResponse_collection = Lens.lens (\CreateCidrCollectionResponse' {collection} -> collection) (\s@CreateCidrCollectionResponse' {} a -> s {collection = a} :: CreateCidrCollectionResponse)

-- | A unique URL that represents the location for the CIDR collection.
createCidrCollectionResponse_location :: Lens.Lens' CreateCidrCollectionResponse (Prelude.Maybe Prelude.Text)
createCidrCollectionResponse_location = Lens.lens (\CreateCidrCollectionResponse' {location} -> location) (\s@CreateCidrCollectionResponse' {} a -> s {location = a} :: CreateCidrCollectionResponse)

-- | The response's http status code.
createCidrCollectionResponse_httpStatus :: Lens.Lens' CreateCidrCollectionResponse Prelude.Int
createCidrCollectionResponse_httpStatus = Lens.lens (\CreateCidrCollectionResponse' {httpStatus} -> httpStatus) (\s@CreateCidrCollectionResponse' {} a -> s {httpStatus = a} :: CreateCidrCollectionResponse)

instance Prelude.NFData CreateCidrCollectionResponse where
  rnf CreateCidrCollectionResponse' {..} =
    Prelude.rnf collection
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf httpStatus
