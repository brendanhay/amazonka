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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsIamAccessKeySessionContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsIamAccessKeySessionContextAttributes
import Amazonka.SecurityHub.Types.AwsIamAccessKeySessionContextSessionIssuer

-- | Provides information about the session that the key was used for.
--
-- /See:/ 'newAwsIamAccessKeySessionContext' smart constructor.
data AwsIamAccessKeySessionContext = AwsIamAccessKeySessionContext'
  { -- | Information about the entity that created the session.
    sessionIssuer :: Prelude.Maybe AwsIamAccessKeySessionContextSessionIssuer,
    -- | Attributes of the session that the key was used for.
    attributes :: Prelude.Maybe AwsIamAccessKeySessionContextAttributes
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
-- 'sessionIssuer', 'awsIamAccessKeySessionContext_sessionIssuer' - Information about the entity that created the session.
--
-- 'attributes', 'awsIamAccessKeySessionContext_attributes' - Attributes of the session that the key was used for.
newAwsIamAccessKeySessionContext ::
  AwsIamAccessKeySessionContext
newAwsIamAccessKeySessionContext =
  AwsIamAccessKeySessionContext'
    { sessionIssuer =
        Prelude.Nothing,
      attributes = Prelude.Nothing
    }

-- | Information about the entity that created the session.
awsIamAccessKeySessionContext_sessionIssuer :: Lens.Lens' AwsIamAccessKeySessionContext (Prelude.Maybe AwsIamAccessKeySessionContextSessionIssuer)
awsIamAccessKeySessionContext_sessionIssuer = Lens.lens (\AwsIamAccessKeySessionContext' {sessionIssuer} -> sessionIssuer) (\s@AwsIamAccessKeySessionContext' {} a -> s {sessionIssuer = a} :: AwsIamAccessKeySessionContext)

-- | Attributes of the session that the key was used for.
awsIamAccessKeySessionContext_attributes :: Lens.Lens' AwsIamAccessKeySessionContext (Prelude.Maybe AwsIamAccessKeySessionContextAttributes)
awsIamAccessKeySessionContext_attributes = Lens.lens (\AwsIamAccessKeySessionContext' {attributes} -> attributes) (\s@AwsIamAccessKeySessionContext' {} a -> s {attributes = a} :: AwsIamAccessKeySessionContext)

instance Core.FromJSON AwsIamAccessKeySessionContext where
  parseJSON =
    Core.withObject
      "AwsIamAccessKeySessionContext"
      ( \x ->
          AwsIamAccessKeySessionContext'
            Prelude.<$> (x Core..:? "SessionIssuer")
            Prelude.<*> (x Core..:? "Attributes")
      )

instance
  Prelude.Hashable
    AwsIamAccessKeySessionContext
  where
  hashWithSalt _salt AwsIamAccessKeySessionContext' {..} =
    _salt `Prelude.hashWithSalt` sessionIssuer
      `Prelude.hashWithSalt` attributes

instance Prelude.NFData AwsIamAccessKeySessionContext where
  rnf AwsIamAccessKeySessionContext' {..} =
    Prelude.rnf sessionIssuer
      `Prelude.seq` Prelude.rnf attributes

instance Core.ToJSON AwsIamAccessKeySessionContext where
  toJSON AwsIamAccessKeySessionContext' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SessionIssuer" Core..=) Prelude.<$> sessionIssuer,
            ("Attributes" Core..=) Prelude.<$> attributes
          ]
      )
