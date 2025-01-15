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
-- Module      : Amazonka.SecurityHub.Types.AwsIamAccessKeySessionContext
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsIamAccessKeySessionContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsIamAccessKeySessionContextAttributes
import Amazonka.SecurityHub.Types.AwsIamAccessKeySessionContextSessionIssuer

-- | Provides information about the session that the key was used for.
--
-- /See:/ 'newAwsIamAccessKeySessionContext' smart constructor.
data AwsIamAccessKeySessionContext = AwsIamAccessKeySessionContext'
  { -- | Attributes of the session that the key was used for.
    attributes :: Prelude.Maybe AwsIamAccessKeySessionContextAttributes,
    -- | Information about the entity that created the session.
    sessionIssuer :: Prelude.Maybe AwsIamAccessKeySessionContextSessionIssuer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsIamAccessKeySessionContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'awsIamAccessKeySessionContext_attributes' - Attributes of the session that the key was used for.
--
-- 'sessionIssuer', 'awsIamAccessKeySessionContext_sessionIssuer' - Information about the entity that created the session.
newAwsIamAccessKeySessionContext ::
  AwsIamAccessKeySessionContext
newAwsIamAccessKeySessionContext =
  AwsIamAccessKeySessionContext'
    { attributes =
        Prelude.Nothing,
      sessionIssuer = Prelude.Nothing
    }

-- | Attributes of the session that the key was used for.
awsIamAccessKeySessionContext_attributes :: Lens.Lens' AwsIamAccessKeySessionContext (Prelude.Maybe AwsIamAccessKeySessionContextAttributes)
awsIamAccessKeySessionContext_attributes = Lens.lens (\AwsIamAccessKeySessionContext' {attributes} -> attributes) (\s@AwsIamAccessKeySessionContext' {} a -> s {attributes = a} :: AwsIamAccessKeySessionContext)

-- | Information about the entity that created the session.
awsIamAccessKeySessionContext_sessionIssuer :: Lens.Lens' AwsIamAccessKeySessionContext (Prelude.Maybe AwsIamAccessKeySessionContextSessionIssuer)
awsIamAccessKeySessionContext_sessionIssuer = Lens.lens (\AwsIamAccessKeySessionContext' {sessionIssuer} -> sessionIssuer) (\s@AwsIamAccessKeySessionContext' {} a -> s {sessionIssuer = a} :: AwsIamAccessKeySessionContext)

instance Data.FromJSON AwsIamAccessKeySessionContext where
  parseJSON =
    Data.withObject
      "AwsIamAccessKeySessionContext"
      ( \x ->
          AwsIamAccessKeySessionContext'
            Prelude.<$> (x Data..:? "Attributes")
            Prelude.<*> (x Data..:? "SessionIssuer")
      )

instance
  Prelude.Hashable
    AwsIamAccessKeySessionContext
  where
  hashWithSalt _salt AwsIamAccessKeySessionContext' {..} =
    _salt
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` sessionIssuer

instance Prelude.NFData AwsIamAccessKeySessionContext where
  rnf AwsIamAccessKeySessionContext' {..} =
    Prelude.rnf attributes `Prelude.seq`
      Prelude.rnf sessionIssuer

instance Data.ToJSON AwsIamAccessKeySessionContext where
  toJSON AwsIamAccessKeySessionContext' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Attributes" Data..=) Prelude.<$> attributes,
            ("SessionIssuer" Data..=) Prelude.<$> sessionIssuer
          ]
      )
