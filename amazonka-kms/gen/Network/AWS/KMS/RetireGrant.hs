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
-- Module      : Network.AWS.KMS.RetireGrant
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retires a grant. To clean up, you can retire a grant when you're done using it. You should revoke a grant when you intend to actively deny operations that depend on it. The following are permitted to call this API:
--
--
--     * The AWS account (root user) under which the grant was created
--
--     * The @RetiringPrincipal@ , if present in the grant
--
--     * The @GranteePrincipal@ , if @RetireGrant@ is an operation specified in the grant
--
--
--
-- You must identify the grant to retire by its grant token or by a combination of the grant ID and the Amazon Resource Name (ARN) of the customer master key (CMK). A grant token is a unique variable-length base64-encoded string. A grant ID is a 64 character unique identifier of a grant. The 'CreateGrant' operation returns both.
--
module Network.AWS.KMS.RetireGrant
    (
    -- * Creating a Request
      retireGrant
    , RetireGrant
    -- * Request Lenses
    , rgKeyId
    , rgGrantId
    , rgGrantToken

    -- * Destructuring the Response
    , retireGrantResponse
    , RetireGrantResponse
    ) where

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'retireGrant' smart constructor.
data RetireGrant = RetireGrant'
  { _rgKeyId      :: !(Maybe Text)
  , _rgGrantId    :: !(Maybe Text)
  , _rgGrantToken :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RetireGrant' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgKeyId' - The Amazon Resource Name (ARN) of the CMK associated with the grant.  For example: @arn:aws:kms:us-east-2:444455556666:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- * 'rgGrantId' - Unique identifier of the grant to retire. The grant ID is returned in the response to a @CreateGrant@ operation.     * Grant ID Example - 0123456789012345678901234567890123456789012345678901234567890123
--
-- * 'rgGrantToken' - Token that identifies the grant to be retired.
retireGrant
    :: RetireGrant
retireGrant =
  RetireGrant'
    {_rgKeyId = Nothing, _rgGrantId = Nothing, _rgGrantToken = Nothing}


-- | The Amazon Resource Name (ARN) of the CMK associated with the grant.  For example: @arn:aws:kms:us-east-2:444455556666:key/1234abcd-12ab-34cd-56ef-1234567890ab@
rgKeyId :: Lens' RetireGrant (Maybe Text)
rgKeyId = lens _rgKeyId (\ s a -> s{_rgKeyId = a})

-- | Unique identifier of the grant to retire. The grant ID is returned in the response to a @CreateGrant@ operation.     * Grant ID Example - 0123456789012345678901234567890123456789012345678901234567890123
rgGrantId :: Lens' RetireGrant (Maybe Text)
rgGrantId = lens _rgGrantId (\ s a -> s{_rgGrantId = a})

-- | Token that identifies the grant to be retired.
rgGrantToken :: Lens' RetireGrant (Maybe Text)
rgGrantToken = lens _rgGrantToken (\ s a -> s{_rgGrantToken = a})

instance AWSRequest RetireGrant where
        type Rs RetireGrant = RetireGrantResponse
        request = postJSON kms
        response = receiveNull RetireGrantResponse'

instance Hashable RetireGrant where

instance NFData RetireGrant where

instance ToHeaders RetireGrant where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.RetireGrant" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RetireGrant where
        toJSON RetireGrant'{..}
          = object
              (catMaybes
                 [("KeyId" .=) <$> _rgKeyId,
                  ("GrantId" .=) <$> _rgGrantId,
                  ("GrantToken" .=) <$> _rgGrantToken])

instance ToPath RetireGrant where
        toPath = const "/"

instance ToQuery RetireGrant where
        toQuery = const mempty

-- | /See:/ 'retireGrantResponse' smart constructor.
data RetireGrantResponse =
  RetireGrantResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RetireGrantResponse' with the minimum fields required to make a request.
--
retireGrantResponse
    :: RetireGrantResponse
retireGrantResponse = RetireGrantResponse'


instance NFData RetireGrantResponse where
