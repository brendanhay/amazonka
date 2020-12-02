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
-- Module      : Network.AWS.CodeStar.DescribeUserProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a user in AWS CodeStar and the user attributes across all projects.
--
--
module Network.AWS.CodeStar.DescribeUserProfile
    (
    -- * Creating a Request
      describeUserProfile
    , DescribeUserProfile
    -- * Request Lenses
    , dupUserARN

    -- * Destructuring the Response
    , describeUserProfileResponse
    , DescribeUserProfileResponse
    -- * Response Lenses
    , duprsSshPublicKey
    , duprsEmailAddress
    , duprsDisplayName
    , duprsResponseStatus
    , duprsUserARN
    , duprsCreatedTimestamp
    , duprsLastModifiedTimestamp
    ) where

import Network.AWS.CodeStar.Types
import Network.AWS.CodeStar.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeUserProfile' smart constructor.
newtype DescribeUserProfile = DescribeUserProfile'
  { _dupUserARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeUserProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dupUserARN' - The Amazon Resource Name (ARN) of the user.
describeUserProfile
    :: Text -- ^ 'dupUserARN'
    -> DescribeUserProfile
describeUserProfile pUserARN_ = DescribeUserProfile' {_dupUserARN = pUserARN_}


-- | The Amazon Resource Name (ARN) of the user.
dupUserARN :: Lens' DescribeUserProfile Text
dupUserARN = lens _dupUserARN (\ s a -> s{_dupUserARN = a})

instance AWSRequest DescribeUserProfile where
        type Rs DescribeUserProfile =
             DescribeUserProfileResponse
        request = postJSON codeStar
        response
          = receiveJSON
              (\ s h x ->
                 DescribeUserProfileResponse' <$>
                   (x .?> "sshPublicKey") <*> (x .?> "emailAddress") <*>
                     (x .?> "displayName")
                     <*> (pure (fromEnum s))
                     <*> (x .:> "userArn")
                     <*> (x .:> "createdTimestamp")
                     <*> (x .:> "lastModifiedTimestamp"))

instance Hashable DescribeUserProfile where

instance NFData DescribeUserProfile where

instance ToHeaders DescribeUserProfile where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeStar_20170419.DescribeUserProfile" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeUserProfile where
        toJSON DescribeUserProfile'{..}
          = object
              (catMaybes [Just ("userArn" .= _dupUserARN)])

instance ToPath DescribeUserProfile where
        toPath = const "/"

instance ToQuery DescribeUserProfile where
        toQuery = const mempty

-- | /See:/ 'describeUserProfileResponse' smart constructor.
data DescribeUserProfileResponse = DescribeUserProfileResponse'
  { _duprsSshPublicKey          :: !(Maybe Text)
  , _duprsEmailAddress          :: !(Maybe (Sensitive Text))
  , _duprsDisplayName           :: !(Maybe Text)
  , _duprsResponseStatus        :: !Int
  , _duprsUserARN               :: !Text
  , _duprsCreatedTimestamp      :: !POSIX
  , _duprsLastModifiedTimestamp :: !POSIX
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeUserProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duprsSshPublicKey' - The SSH public key associated with the user. This SSH public key is associated with the user profile, and can be used in conjunction with the associated private key for access to project resources, such as Amazon EC2 instances, if a project owner grants remote access to those resources.
--
-- * 'duprsEmailAddress' - The email address for the user. Optional.
--
-- * 'duprsDisplayName' - The display name shown for the user in AWS CodeStar projects. For example, this could be set to both first and last name ("Mary Major") or a single name ("Mary"). The display name is also used to generate the initial icon associated with the user in AWS CodeStar projects. If spaces are included in the display name, the first character that appears after the space will be used as the second character in the user initial icon. The initial icon displays a maximum of two characters, so a display name with more than one space (for example "Mary Jane Major") would generate an initial icon using the first character and the first character after the space ("MJ", not "MM").
--
-- * 'duprsResponseStatus' - -- | The response status code.
--
-- * 'duprsUserARN' - The Amazon Resource Name (ARN) of the user.
--
-- * 'duprsCreatedTimestamp' - The date and time when the user profile was created in AWS CodeStar, in timestamp format.
--
-- * 'duprsLastModifiedTimestamp' - The date and time when the user profile was last modified, in timestamp format.
describeUserProfileResponse
    :: Int -- ^ 'duprsResponseStatus'
    -> Text -- ^ 'duprsUserARN'
    -> UTCTime -- ^ 'duprsCreatedTimestamp'
    -> UTCTime -- ^ 'duprsLastModifiedTimestamp'
    -> DescribeUserProfileResponse
describeUserProfileResponse pResponseStatus_ pUserARN_ pCreatedTimestamp_ pLastModifiedTimestamp_ =
  DescribeUserProfileResponse'
    { _duprsSshPublicKey = Nothing
    , _duprsEmailAddress = Nothing
    , _duprsDisplayName = Nothing
    , _duprsResponseStatus = pResponseStatus_
    , _duprsUserARN = pUserARN_
    , _duprsCreatedTimestamp = _Time # pCreatedTimestamp_
    , _duprsLastModifiedTimestamp = _Time # pLastModifiedTimestamp_
    }


-- | The SSH public key associated with the user. This SSH public key is associated with the user profile, and can be used in conjunction with the associated private key for access to project resources, such as Amazon EC2 instances, if a project owner grants remote access to those resources.
duprsSshPublicKey :: Lens' DescribeUserProfileResponse (Maybe Text)
duprsSshPublicKey = lens _duprsSshPublicKey (\ s a -> s{_duprsSshPublicKey = a})

-- | The email address for the user. Optional.
duprsEmailAddress :: Lens' DescribeUserProfileResponse (Maybe Text)
duprsEmailAddress = lens _duprsEmailAddress (\ s a -> s{_duprsEmailAddress = a}) . mapping _Sensitive

-- | The display name shown for the user in AWS CodeStar projects. For example, this could be set to both first and last name ("Mary Major") or a single name ("Mary"). The display name is also used to generate the initial icon associated with the user in AWS CodeStar projects. If spaces are included in the display name, the first character that appears after the space will be used as the second character in the user initial icon. The initial icon displays a maximum of two characters, so a display name with more than one space (for example "Mary Jane Major") would generate an initial icon using the first character and the first character after the space ("MJ", not "MM").
duprsDisplayName :: Lens' DescribeUserProfileResponse (Maybe Text)
duprsDisplayName = lens _duprsDisplayName (\ s a -> s{_duprsDisplayName = a})

-- | -- | The response status code.
duprsResponseStatus :: Lens' DescribeUserProfileResponse Int
duprsResponseStatus = lens _duprsResponseStatus (\ s a -> s{_duprsResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the user.
duprsUserARN :: Lens' DescribeUserProfileResponse Text
duprsUserARN = lens _duprsUserARN (\ s a -> s{_duprsUserARN = a})

-- | The date and time when the user profile was created in AWS CodeStar, in timestamp format.
duprsCreatedTimestamp :: Lens' DescribeUserProfileResponse UTCTime
duprsCreatedTimestamp = lens _duprsCreatedTimestamp (\ s a -> s{_duprsCreatedTimestamp = a}) . _Time

-- | The date and time when the user profile was last modified, in timestamp format.
duprsLastModifiedTimestamp :: Lens' DescribeUserProfileResponse UTCTime
duprsLastModifiedTimestamp = lens _duprsLastModifiedTimestamp (\ s a -> s{_duprsLastModifiedTimestamp = a}) . _Time

instance NFData DescribeUserProfileResponse where
