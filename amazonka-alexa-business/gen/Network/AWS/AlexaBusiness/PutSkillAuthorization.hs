{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.PutSkillAuthorization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Links a user's account to a third-party skill provider. If this API operation is called by an assumed IAM role, the skill being linked must be a private skill. Also, the skill must be owned by the AWS account that assumed the IAM role.
--
--
module Network.AWS.AlexaBusiness.PutSkillAuthorization
    (
    -- * Creating a Request
      putSkillAuthorization
    , PutSkillAuthorization
    -- * Request Lenses
    , psaRoomARN
    , psaAuthorizationResult
    , psaSkillId

    -- * Destructuring the Response
    , putSkillAuthorizationResponse
    , PutSkillAuthorizationResponse
    -- * Response Lenses
    , psarsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putSkillAuthorization' smart constructor.
data PutSkillAuthorization = PutSkillAuthorization'
  { _psaRoomARN             :: !(Maybe Text)
  , _psaAuthorizationResult :: !(Sensitive (Map Text Text))
  , _psaSkillId             :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutSkillAuthorization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psaRoomARN' - The room that the skill is authorized for.
--
-- * 'psaAuthorizationResult' - The authorization result specific to OAUTH code grant output. "Code
